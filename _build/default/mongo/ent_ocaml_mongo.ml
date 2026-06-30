type config = {
  database : string;
}

type ctx = {
  client : Obj.t;
  config : config;
}

let create ~client config = { client = Obj.repr client; config }

let rec value_to_bson = function
  | Ent_ocaml.V_string value -> Ok (Bson.create_string value)
  | V_int value -> Ok (Bson.create_int64 (Int64.of_int value))
  | V_int32 value -> Ok (Bson.create_int32 value)
  | V_int64 value -> Ok (Bson.create_int64 value)
  | V_float value -> Ok (Bson.create_double value)
  | V_bool value -> Ok (Bson.create_boolean value)
  | V_null -> Ok (Bson.create_null ())
  | V_list values ->
      let rec loop acc = function
        | [] -> Ok (Bson.create_list (List.rev acc))
        | value :: rest -> (
            match value_to_bson value with
            | Ok bson -> loop (bson :: acc) rest
            | Error _ as error -> error)
      in
      loop [] values
  | V_doc fields ->
      let rec loop doc = function
        | [] -> Ok (Bson.create_doc_element doc)
        | (name, value) :: rest -> (
            match value_to_bson value with
            | Ok bson -> loop (Bson.add_element name bson doc) rest
            | Error _ as error -> error)
      in
      loop Bson.empty fields

let op_doc field op value =
  match value_to_bson value with
  | Error _ as error -> error
  | Ok bson -> Ok (Bson.add_element field (Bson.create_doc_element (Bson.add_element op bson Bson.empty)) Bson.empty)

let doc fields =
  List.fold_left
    (fun acc (name, value) -> Bson.add_element name value acc)
    Bson.empty fields

let rec predicate_to_bson = function
  | Ent_ocaml.Eq (field, value) -> (
      match value_to_bson value with
      | Ok bson -> Ok (doc [ (field, bson) ])
      | Error _ as error -> error)
  | Neq (field, value) -> op_doc field "$ne" value
  | Gt (field, value) -> op_doc field "$gt" value
  | Gte (field, value) -> op_doc field "$gte" value
  | Lt (field, value) -> op_doc field "$lt" value
  | Lte (field, value) -> op_doc field "$lte" value
  | In (field, values) -> op_doc field "$in" (V_list values)
  | Not_in (field, values) -> op_doc field "$nin" (V_list values)
  | Is_nil field -> Ok (doc [ (field, Bson.create_null ()) ])
  | Not_nil field ->
      Ok
        (doc
           [
             ( field,
               Bson.create_doc_element
                 (doc [ ("$ne", Bson.create_null ()) ]) );
           ])
  | And predicates -> logical "$and" predicates
  | Or predicates -> logical "$or" predicates
  | Not predicate -> (
      match predicate_to_bson predicate with
      | Ok bson -> Ok (doc [ ("$nor", Bson.create_list [ Bson.create_doc_element bson ]) ])
      | Error _ as error -> error)
  | Contains (field, value) ->
      Ok (doc [ (field, Bson.create_string (".*" ^ value ^ ".*")) ])
  | Has_prefix (field, value) ->
      Ok (doc [ (field, Bson.create_string ("^" ^ value)) ])
  | Has_suffix (field, value) ->
      Ok (doc [ (field, Bson.create_string (value ^ "$")) ])
  | Has_edge _ | Has_edge_with _ ->
      Error (`Bad_query "edge predicates are not implemented in the Mongo backend yet")
  | Backend (_, _) ->
      Error (`Bad_query "backend predicates need typed backend-specific support")

and logical op predicates =
  let rec loop acc = function
    | [] -> Ok (doc [ (op, Bson.create_list (List.rev acc)) ])
    | predicate :: rest -> (
        match predicate_to_bson predicate with
        | Ok bson -> loop (Bson.create_doc_element bson :: acc) rest
        | Error _ as error -> error)
  in
  loop [] predicates
