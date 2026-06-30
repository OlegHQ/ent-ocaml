type field_type =
  | String
  | Int
  | Int32
  | Int64
  | Float
  | Bool
  | Time_ms
  | Uuid
  | Bytes
  | Json
  | Enum of string list
  | List of field_type
  | Option of field_type
  | Custom of string

type value =
  | V_string of string
  | V_int of int
  | V_int32 of int32
  | V_int64 of int64
  | V_float of float
  | V_bool of bool
  | V_null
  | V_list of value list
  | V_doc of (string * value) list

type field = {
  name : string;
  storage_key : string;
  typ : field_type;
  required : bool;
  unique : bool;
  immutable : bool;
  nillable : bool;
  validators : (value -> (unit, string) result) list;
}

type edge_cardinality = One | Many
type edge_direction = To | From of { ref_name : string }

type edge = {
  name : string;
  target : string;
  direction : edge_direction;
  cardinality : edge_cardinality;
  required : bool;
  storage_key : string option;
}

type index = {
  name : string option;
  fields : string list;
  edges : string list;
  unique : bool;
}

type entity = {
  name : string;
  collection : string;
  fields : field list;
  edges : edge list;
  indexes : index list;
}

type predicate =
  | Eq of string * value
  | Neq of string * value
  | Gt of string * value
  | Gte of string * value
  | Lt of string * value
  | Lte of string * value
  | In of string * value list
  | Not_in of string * value list
  | Contains of string * string
  | Has_prefix of string * string
  | Has_suffix of string * string
  | Is_nil of string
  | Not_nil of string
  | And of predicate list
  | Or of predicate list
  | Not of predicate
  | Has_edge of string
  | Has_edge_with of string * predicate list
  | Backend of string * value

type order_direction = Asc | Desc

type order = {
  field : string;
  direction : order_direction;
}

type query = {
  entity : entity;
  predicates : predicate list;
  select : string list;
  orders : order list;
  limit : int option;
  offset : int option;
}

module Query = struct
  let make ?(where = []) ?(select = []) ?(order = []) ?limit ?offset entity =
    { entity; predicates = where; select; orders = order; limit; offset }

  let where predicate query =
    { query with predicates = query.predicates @ [ predicate ] }

  let where_all predicates query =
    { query with predicates = query.predicates @ predicates }

  let select fields query = { query with select = fields }
  let order_by orders query = { query with orders }
  let limit limit query = { query with limit = Some limit }
  let offset offset query = { query with offset = Some offset }
end

type mutation_op = Create | Update_one | Update | Delete_one | Delete

type mutation = {
  entity : entity;
  op : mutation_op;
  predicates : predicate list;
  set : (string * value) list;
  clear : string list;
  add : (string * value) list;
}

module Mutation = struct
  let field_name = fst

  let remove_field name fields =
    List.filter (fun (field, _) -> field <> name) fields

  let set field mutation =
    let name = field_name field in
    {
      mutation with
      set = remove_field name mutation.set @ [ field ];
      add = remove_field name mutation.add;
      clear = List.filter (( <> ) name) mutation.clear;
    }

  let set_all fields mutation =
    List.fold_left (fun mutation field -> set field mutation) mutation fields

  let clear name mutation =
    {
      mutation with
      set = remove_field name mutation.set;
      add = remove_field name mutation.add;
      clear =
        if List.mem name mutation.clear then mutation.clear
        else mutation.clear @ [ name ];
    }

  let add field mutation =
    let name = field_name field in
    {
      mutation with
      add = remove_field name mutation.add @ [ field ];
      clear = List.filter (( <> ) name) mutation.clear;
    }
end

type error =
  [ `Backend of string
  | `Bad_schema of string
  | `Bad_query of string
  | `Decode of string
  | `Denied of string
  | `Not_found
  | `Not_singular
  | `Constraint of string ]

let error_to_string = function
  | `Backend message -> "backend: " ^ message
  | `Bad_schema message -> "bad schema: " ^ message
  | `Bad_query message -> "bad query: " ^ message
  | `Decode message -> "decode: " ^ message
  | `Denied message -> "denied: " ^ message
  | `Not_found -> "not found"
  | `Not_singular -> "not singular"
  | `Constraint message -> "constraint: " ^ message

module Result_syntax = struct
  let ( let* ) result f = match result with Ok value -> f value | Error _ as error -> error
  let ( let+ ) result f = match result with Ok value -> Ok (f value) | Error _ as error -> error
end

let find_field (entity : entity) name =
  List.find_opt (fun (field : field) -> field.name = name) entity.fields

let rec has_duplicate = function
  | [] -> None
  | name :: rest ->
      if List.mem name rest then Some name else has_duplicate rest

let validate_known_fields (entity : entity) context names =
  match List.find_opt (fun name -> Option.is_none (find_field entity name)) names with
  | Some name -> Error (`Bad_query (context ^ " field not found: " ^ name))
  | None -> Ok ()

let validate_no_duplicate context names =
  match has_duplicate names with
  | None -> Ok ()
  | Some name -> Error (`Bad_query ("duplicate " ^ context ^ " field: " ^ name))

let validate_no_overlap left_name left right_name right =
  match List.find_opt (fun name -> List.mem name right) left with
  | None -> Ok ()
  | Some name ->
      Error
        (`Bad_query
          (Printf.sprintf "field %s appears in both %s and %s" name left_name
             right_name))

let validate_required_create (entity : entity) set =
  let set_names = List.map fst set in
  let missing =
    List.find_opt
      (fun (field : field) -> field.required && not (List.mem field.name set_names))
      entity.fields
  in
  match missing with
  | Some field -> Error (`Bad_query ("missing required field: " ^ field.name))
  | None -> (
      match
        List.find_opt
          (fun (name, value) ->
            match (find_field entity name, value) with
            | Some field, V_null -> field.required && not field.nillable
            | _ -> false)
          set
      with
      | Some (name, _) ->
          Error (`Bad_query ("required field is null: " ^ name))
      | None -> Ok ())

let validate_immutable_update (entity : entity) fields =
  match
    List.find_opt
      (fun name ->
        match find_field entity name with
        | Some field -> field.immutable
        | None -> false)
      fields
  with
  | None -> Ok ()
  | Some name -> Error (`Bad_query ("immutable field cannot be updated: " ^ name))

let validate_field_values (entity : entity) values =
  let validate_one (name, value) =
    match find_field entity name with
    | None -> Ok ()
    | Some field ->
        let rec loop = function
          | [] -> Ok ()
          | validator :: validators -> (
              match validator value with
              | Ok () -> loop validators
              | Error message ->
                  Error
                    (`Bad_query
                      ("validation failed for field " ^ name ^ ": " ^ message)))
        in
        loop field.validators
  in
  let rec loop = function
    | [] -> Ok ()
    | value :: values -> (
        match validate_one value with
        | Ok () -> loop values
        | Error _ as error -> error)
  in
  loop values

let ( let* ) result f = match result with Ok value -> f value | Error _ as error -> error

let validate_mutation mutation =
  let entity = mutation.entity in
  let set_names = List.map fst mutation.set in
  let add_names = List.map fst mutation.add in
  let clear_names = mutation.clear in
  let all_names = set_names @ add_names @ clear_names in
  let* () = validate_known_fields entity "mutation" all_names in
  let* () = validate_no_duplicate "set" set_names in
  let* () = validate_no_duplicate "add" add_names in
  let* () = validate_no_duplicate "clear" clear_names in
  let* () = validate_no_overlap "set" set_names "add" add_names in
  let* () = validate_no_overlap "set" set_names "clear" clear_names in
  let* () = validate_no_overlap "add" add_names "clear" clear_names in
  let* () = validate_field_values entity (mutation.set @ mutation.add) in
  match mutation.op with
  | Create ->
      let* () = validate_required_create entity mutation.set in
      if mutation.predicates <> [] then
        Error (`Bad_query "create mutation cannot have predicates")
      else if mutation.clear <> [] || mutation.add <> [] then
        Error (`Bad_query "create mutation only supports set fields")
      else Ok ()
  | Update_one | Update ->
      validate_immutable_update entity all_names
  | Delete_one | Delete ->
      if all_names = [] then Ok ()
      else Error (`Bad_query "delete mutation cannot set, add, or clear fields")

type privacy_decision = Allow | Deny of string | Skip
type 'ctx query_rule = 'ctx -> query -> privacy_decision
type 'ctx mutation_rule = 'ctx -> mutation -> privacy_decision

module type BACKEND = sig
  type ctx
  type doc
  type tx

  val find : ctx -> query -> (doc list, error) result
  val insert : ctx -> entity -> doc -> (doc, error) result
  val insert_many : ctx -> entity -> doc list -> (doc list, error) result
  val update : ctx -> mutation -> (int, error) result
  val delete : ctx -> mutation -> (int, error) result
  val count : ctx -> query -> (int, error) result
  val transaction : ctx -> (tx -> ('a, error) result) -> ('a, error) result
end

module type STORE_BACKEND = sig
  type ctx
  type doc

  val find_as :
    ctx ->
    query ->
    decode:(doc -> ('a, string) result) ->
    ('a list, error) result

  val find_one_as :
    ctx ->
    query ->
    decode:(doc -> ('a, string) result) ->
    ('a option, error) result

  val insert_values : ctx -> mutation -> (doc, error) result

  val insert_many_values :
    ?ordered:bool -> ctx -> mutation list -> (doc list, error) result

  val update_one : ctx -> mutation -> (unit, error) result
  val update : ctx -> mutation -> (int, error) result
  val delete : ctx -> mutation -> (int, error) result
  val count : ctx -> query -> (int, error) result
end
