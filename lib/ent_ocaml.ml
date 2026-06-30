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
  | Json_eq of string * string list * value
  | Json_neq of string * string list * value
  | Json_gt of string * string list * value
  | Json_gte of string * string list * value
  | Json_lt of string * string list * value
  | Json_lte of string * string list * value
  | Json_in of string * string list * value list
  | Json_not_in of string * string list * value list
  | Json_is_nil of string * string list
  | Json_not_nil of string * string list
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

type cursor_term = {
  field : string;
  direction : order_direction;
  value : value;
}

type query = {
  entity : entity;
  predicates : predicate list;
  select : string list;
  orders : order list;
  limit : int option;
  offset : int option;
}

type edge_query = {
  source : query;
  edge : string;
  target : entity;
  target_query : query;
}

type aggregate_op = Count | Min of string | Max of string | Sum of string | Avg of string

type aggregate = {
  query : query;
  op : aggregate_op;
}

type aggregate_scan = {
  query : query;
  ops : (string * aggregate_op) list;
}

type group_aggregate = {
  aggregate : aggregate;
  group : string;
}

type group_result = {
  group : value;
  value : value option;
}

module Schema_snapshot = struct
  let string name value = (name, V_string value)
  let bool name value = (name, V_bool value)

  let rec field_type = function
    | String -> V_string "string"
    | Int -> V_string "int"
    | Int32 -> V_string "int32"
    | Int64 -> V_string "int64"
    | Float -> V_string "float"
    | Bool -> V_string "bool"
    | Time_ms -> V_string "time_ms"
    | Uuid -> V_string "uuid"
    | Bytes -> V_string "bytes"
    | Json -> V_string "json"
    | Enum values -> V_doc [ ("kind", V_string "enum"); ("values", V_list (List.map (fun value -> V_string value) values)) ]
    | List typ -> V_doc [ ("kind", V_string "list"); ("type", field_type typ) ]
    | Option typ -> V_doc [ ("kind", V_string "option"); ("type", field_type typ) ]
    | Custom name -> V_doc [ ("kind", V_string "custom"); ("name", V_string name) ]

  let field (field : field) =
    V_doc
      [
        string "name" field.name;
        string "storage_key" field.storage_key;
        ("type", field_type field.typ);
        bool "required" field.required;
        bool "unique" field.unique;
        bool "immutable" field.immutable;
        bool "nillable" field.nillable;
      ]

  let edge_direction = function
    | To -> V_string "to"
    | From { ref_name } -> V_doc [ ("kind", V_string "from"); ("ref_name", V_string ref_name) ]

  let edge_cardinality = function
    | One -> V_string "one"
    | Many -> V_string "many"

  let option_string = function
    | None -> V_null
    | Some value -> V_string value

  let edge (edge : edge) =
    V_doc
      [
        string "name" edge.name;
        string "target" edge.target;
        ("direction", edge_direction edge.direction);
        ("cardinality", edge_cardinality edge.cardinality);
        bool "required" edge.required;
        ("storage_key", option_string edge.storage_key);
      ]

  let index (index : index) =
    V_doc
      [
        ("name", option_string index.name);
        ("fields", V_list (List.map (fun field -> V_string field) index.fields));
        ("edges", V_list (List.map (fun edge -> V_string edge) index.edges));
        bool "unique" index.unique;
      ]

  let entity (entity : entity) =
    V_doc
      [
        ("version", V_int 1);
        string "name" entity.name;
        string "collection" entity.collection;
        ("fields", V_list (List.map field entity.fields));
        ("edges", V_list (List.map edge entity.edges));
        ("indexes", V_list (List.map index entity.indexes));
      ]

  let entities entities =
    V_doc
      [
        ("version", V_int 1);
        ("entities", V_list (List.map entity entities));
      ]
end

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

  let ensure_order (order : order) (query : query) =
    if List.exists (fun (existing : order) -> existing.field = order.field) query.orders
    then query
    else { query with orders = query.orders @ [ order ] }

  let ensure_cursor_orders (terms : cursor_term list) query =
    List.fold_left
      (fun query term ->
        ensure_order
          ({ field = term.field; direction = term.direction } : order)
          query)
      query terms

  let cursor_compare ~after (term : cursor_term) =
    match (after, term.direction) with
    | true, Asc | false, Desc -> Gt (term.field, term.value)
    | true, Desc | false, Asc -> Lt (term.field, term.value)

  let cursor_predicate ~after (terms : cursor_term list) =
    let rec loop equals = function
      | [] -> []
      | term :: rest ->
          let comparison = cursor_compare ~after term in
          let branch =
            match List.rev equals with
            | [] -> comparison
            | equals -> And (equals @ [ comparison ])
          in
          branch :: loop (Eq (term.field, term.value) :: equals) rest
    in
    match loop [] terms with
    | [] -> None
    | [ predicate ] -> Some predicate
    | predicates -> Some (Or predicates)

  let seek ~after terms query =
    let query = ensure_cursor_orders terms query in
    match cursor_predicate ~after terms with
    | None -> query
    | Some predicate -> where predicate query

  let after ~field ~direction value query =
    let predicate =
      match direction with
      | Asc -> Gt (field, value)
      | Desc -> Lt (field, value)
    in
    query
    |> where predicate
    |> ensure_order { field; direction }

  let before ~field ~direction value query =
    let predicate =
      match direction with
      | Asc -> Lt (field, value)
      | Desc -> Gt (field, value)
    in
    query
    |> where predicate
    |> ensure_order { field; direction }

  let after_cursor terms query = seek ~after:true terms query
  let before_cursor terms query = seek ~after:false terms query
end

module Edge_query = struct
  let make ?target_query ~edge ~target source =
    let target_query =
      Option.value target_query ~default:(Query.make target)
    in
    { source; edge; target; target_query }
end

module Aggregate = struct
  let count query = { query; op = Count }
  let min field query = { query; op = Min field }
  let max field query = { query; op = Max field }
  let sum field query = { query; op = Sum field }
  let avg field query = { query; op = Avg field }
  let count_as name = (name, Count)
  let min_as name field = (name, Min field)
  let max_as name field = (name, Max field)
  let sum_as name field = (name, Sum field)
  let avg_as name field = (name, Avg field)
  let scan ops query = { query; ops }
  let group_by group aggregate = { aggregate; group }
end

type mutation_op = Create | Update_one | Update | Delete_one | Delete | Upsert_one

type mutation = {
  entity : entity;
  op : mutation_op;
  predicates : predicate list;
  set : (string * value) list;
  clear : string list;
  add : (string * value) list;
  on_insert : (string * value) list;
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

  let on_insert field mutation =
    let name = field_name field in
    { mutation with on_insert = remove_field name mutation.on_insert @ [ field ] }

  let on_insert_all fields mutation =
    List.fold_left
      (fun mutation field -> on_insert field mutation)
      mutation fields
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

module Dynamic_filter = struct
  type op =
    | Equal
    | Not_equal
    | Greater_than
    | Greater_or_equal
    | Less_than
    | Less_or_equal
    | In_list
    | Not_in_list
    | Contains
    | Has_prefix
    | Has_suffix
    | Is_null
    | Not_null

  type t = {
    field : string;
    op : op;
    value : value option;
  }

  let make ?value ~field op = { field; op; value }

  let rec unwrap_option = function
    | Option typ -> unwrap_option typ
    | typ -> typ

  let rec value_matches typ value =
    match (typ, value) with
    | Option _, V_null -> true
    | Option typ, value -> value_matches typ value
    | List typ, V_list values -> List.for_all (value_matches typ) values
    | String, V_string _ -> true
    | Int, V_int _ -> true
    | Int32, V_int32 _ -> true
    | Int64, V_int64 _ -> true
    | Time_ms, V_int64 _ -> true
    | Float, V_float _ -> true
    | Bool, V_bool _ -> true
    | Uuid, V_string _ -> true
    | Bytes, V_string _ -> true
    | Json, _ -> true
    | Enum values, V_string value -> List.mem value values
    | Custom _, _ -> true
    | _ -> false

  let comparable = function
    | String | Int | Int32 | Int64 | Time_ms | Float | Uuid | Enum _ -> true
    | Bool | Bytes | Json | List _ | Option _ | Custom _ -> false

  let string_like = function
    | String | Uuid | Bytes | Enum _ -> true
    | Int | Int32 | Int64 | Float | Bool | Time_ms | Json | List _ | Option _
    | Custom _ ->
        false

  let field_error entity field =
    `Bad_query ("dynamic filter field not found on " ^ entity.name ^ ": " ^ field)

  let value_error (field : field) =
    `Bad_query ("dynamic filter value has wrong type for field: " ^ field.name)

  let missing_value (field : field) =
    `Bad_query ("dynamic filter requires value for field: " ^ field.name)

  let unexpected_value (field : field) =
    `Bad_query ("dynamic filter does not accept value for field: " ^ field.name)

  let bad_operator (field : field) =
    `Bad_query ("dynamic filter operator is not valid for field: " ^ field.name)

  let value (field : field) = function
    | None -> Error (missing_value field)
    | Some value ->
        if value_matches field.typ value then Ok value else Error (value_error field)

  let list_value (field : field) = function
    | None -> Error (missing_value field)
    | Some (V_list values) ->
        if List.for_all (value_matches field.typ) values then Ok values
        else Error (value_error field)
    | Some _ -> Error (value_error field)

  let no_value (field : field) = function
    | None -> Ok ()
    | Some _ -> Error (unexpected_value field)

  let predicate entity filter =
    let open Result_syntax in
    match find_field entity filter.field with
    | None -> Error (field_error entity filter.field)
    | Some field -> (
        match filter.op with
        | Equal ->
            let+ value = value field filter.value in
            Eq (field.name, value)
        | Not_equal ->
            let+ value = value field filter.value in
            Neq (field.name, value)
        | Greater_than | Greater_or_equal | Less_than | Less_or_equal ->
            if comparable (unwrap_option field.typ) then
              let+ value = value field filter.value in
              match filter.op with
              | Greater_than -> Gt (field.name, value)
              | Greater_or_equal -> Gte (field.name, value)
              | Less_than -> Lt (field.name, value)
              | Less_or_equal -> Lte (field.name, value)
              | _ -> assert false
            else Error (bad_operator field)
        | In_list ->
            let+ values = list_value field filter.value in
            In (field.name, values)
        | Not_in_list ->
            let+ values = list_value field filter.value in
            Not_in (field.name, values)
        | Contains | Has_prefix | Has_suffix -> (
            if string_like (unwrap_option field.typ) then
              match filter.value with
              | Some (V_string value) ->
                  Ok
                    (match filter.op with
                    | Contains -> Contains (field.name, value)
                    | Has_prefix -> Has_prefix (field.name, value)
                    | Has_suffix -> Has_suffix (field.name, value)
                    | _ -> assert false)
              | None -> Error (missing_value field)
              | Some _ -> Error (value_error field)
            else Error (bad_operator field))
        | Is_null ->
            let* () = no_value field filter.value in
            if field.nillable then Ok (Is_nil field.name) else Error (bad_operator field)
        | Not_null ->
            let* () = no_value field filter.value in
            if field.nillable then Ok (Not_nil field.name) else Error (bad_operator field))

  let where filter (query : query) =
    let open Result_syntax in
    let+ predicate = predicate query.entity filter in
    Query.where predicate query

  let where_all filters (query : query) =
    let rec loop query = function
      | [] -> Ok query
      | filter :: rest -> (
          match where filter query with
          | Ok query -> loop query rest
          | Error _ as error -> error)
    in
    loop query filters
end

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

let validate_required_upsert (entity : entity) set on_insert =
  validate_required_create entity (set @ on_insert)

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
  let on_insert_names = List.map fst mutation.on_insert in
  let all_names = set_names @ add_names @ clear_names @ on_insert_names in
  let* () = validate_known_fields entity "mutation" all_names in
  let* () = validate_no_duplicate "set" set_names in
  let* () = validate_no_duplicate "add" add_names in
  let* () = validate_no_duplicate "clear" clear_names in
  let* () = validate_no_duplicate "on_insert" on_insert_names in
  let* () = validate_no_overlap "set" set_names "add" add_names in
  let* () = validate_no_overlap "set" set_names "clear" clear_names in
  let* () = validate_no_overlap "add" add_names "clear" clear_names in
  let* () =
    validate_no_overlap "set" set_names "on_insert" on_insert_names
  in
  let* () =
    validate_no_overlap "add" add_names "on_insert" on_insert_names
  in
  let* () =
    validate_no_overlap "clear" clear_names "on_insert" on_insert_names
  in
  let* () =
    validate_field_values entity
      (mutation.set @ mutation.add @ mutation.on_insert)
  in
  match mutation.op with
  | Create ->
      let* () = validate_required_create entity mutation.set in
      if mutation.predicates <> [] then
        Error (`Bad_query "create mutation cannot have predicates")
      else if mutation.clear <> [] || mutation.add <> [] || mutation.on_insert <> []
      then
        Error (`Bad_query "create mutation only supports set fields")
      else Ok ()
  | Update_one | Update ->
      if mutation.on_insert <> [] then
        Error (`Bad_query "update mutation cannot have on_insert fields")
      else validate_immutable_update entity all_names
  | Upsert_one ->
      let* () =
        validate_required_upsert entity mutation.set mutation.on_insert
      in
      validate_immutable_update entity (set_names @ add_names @ clear_names)
  | Delete_one | Delete ->
      if all_names = [] then Ok ()
      else Error (`Bad_query "delete mutation cannot set, add, or clear fields")

type privacy_decision = Allow | Deny of string | Skip
type 'ctx query_rule = 'ctx -> query -> privacy_decision
type 'ctx mutation_rule = 'ctx -> mutation -> privacy_decision
type ('ctx, 'a) query_executor = 'ctx -> query -> ('a, error) result
type ('ctx, 'a) mutation_executor = 'ctx -> mutation -> ('a, error) result

type 'ctx query_interceptor = {
  wrap_query : 'a. ('ctx, 'a) query_executor -> 'ctx -> query -> ('a, error) result;
}

type 'ctx mutation_hook = {
  wrap_mutation :
    'a. ('ctx, 'a) mutation_executor -> 'ctx -> mutation -> ('a, error) result;
}

module Privacy = struct
  let evaluate rules ctx value =
    let rec loop saw_rule = function
      | [] ->
          if saw_rule then Error (`Denied "privacy rule chain skipped")
          else Ok ()
      | rule :: rest -> (
          match rule ctx value with
          | Allow -> Ok ()
          | Deny message -> Error (`Denied message)
          | Skip -> loop true rest)
    in
    loop false rules

  let evaluate_query ctx rules query = evaluate rules ctx query
  let evaluate_mutation ctx rules mutation = evaluate rules ctx mutation

  let evaluate_mutations ctx rules mutations =
    let rec loop = function
      | [] -> Ok ()
      | mutation :: rest -> (
          match evaluate_mutation ctx rules mutation with
          | Ok () -> loop rest
          | Error _ as error -> error)
    in
    loop mutations
end

module Interceptor = struct
  let run_query interceptors next =
    let wrapped =
      List.fold_right
        (fun interceptor next ctx query -> interceptor.wrap_query next ctx query)
        interceptors next
    in
    wrapped

  let run_query_value interceptors ctx query =
    run_query interceptors (fun _ query -> Ok query) ctx query
end

module Hook = struct
  let run_mutation hooks next =
    let wrapped =
      List.fold_right
        (fun hook next ctx mutation -> hook.wrap_mutation next ctx mutation)
        hooks next
    in
    wrapped

  let run_mutation_value hooks ctx mutation =
    run_mutation hooks (fun _ mutation -> Ok mutation) ctx mutation

  let run_mutations hooks ctx mutations =
    let rec loop acc = function
      | [] -> Ok (List.rev acc)
      | mutation :: rest -> (
          match run_mutation_value hooks ctx mutation with
          | Ok mutation -> loop (mutation :: acc) rest
          | Error _ as error -> error)
    in
    loop [] mutations
end

module type BACKEND = sig
  type ctx
  type doc
  type tx

  val find : ctx -> query -> (doc list, error) result
  val insert : ctx -> entity -> doc -> (doc, error) result
  val insert_many : ctx -> entity -> doc list -> (doc list, error) result
  val update : ctx -> mutation -> (int, error) result
  val upsert_one : ctx -> mutation -> (unit, error) result
  val delete : ctx -> mutation -> (int, error) result
  val aggregate : ctx -> aggregate -> (value option, error) result
  val aggregate_scan :
    ctx -> aggregate_scan -> ((string * value option) list, error) result
  val group : ctx -> group_aggregate -> (group_result list, error) result
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

  val traverse_as :
    ctx ->
    edge_query ->
    decode:(doc -> ('a, string) result) ->
    ('a list, error) result

  val load_edge_as :
    ctx ->
    edge_query ->
    decode_source:(doc -> ('source, string) result) ->
    decode_target:(doc -> ('target, string) result) ->
    (('source * 'target option) list, error) result

  val insert_values : ctx -> mutation -> (doc, error) result

  val insert_many_values :
    ?ordered:bool -> ctx -> mutation list -> (doc list, error) result

  val update_one : ctx -> mutation -> (unit, error) result
  val update : ctx -> mutation -> (int, error) result
  val upsert_one : ctx -> mutation -> (unit, error) result
  val delete : ctx -> mutation -> (int, error) result
  val aggregate : ctx -> aggregate -> (value option, error) result
  val aggregate_scan :
    ctx -> aggregate_scan -> ((string * value option) list, error) result
  val group : ctx -> group_aggregate -> (group_result list, error) result
  val count : ctx -> query -> (int, error) result
  val transaction : ctx -> (ctx -> ('a, error) result) -> ('a, error) result
end
