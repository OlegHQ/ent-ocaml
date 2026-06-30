type config = {
  database : string;
}

type transaction_state = {
  session : Mongo_session.t;
  txn_number : int64;
  options : Ent_ocaml.transaction_options option;
  mutable started : bool;
}

type ctx = {
  client : Mongo_eio.direct_client;
  config : config;
  transaction : transaction_state option;
}

type doc = Bson.t

type index_check_status =
  | Present
  | Missing
  | Mismatched of string list

type index_check = {
  entity : string;
  collection : string;
  name : string;
  status : index_check_status;
}

type collection_validator_check_status =
  | Validator_present
  | Validator_missing
  | Validator_mismatched of string list

type collection_validator_check = {
  validator_entity : string;
  validator_collection : string;
  validator_status : collection_validator_check_status;
}

let create ~client config = { client; config; transaction = None }

module Order = struct
  let expression ?as_ ~name ~direction expression =
    Ent_ocaml.Order.backend ?as_ ~backend:"mongo" ~name ~direction expression
end

let transaction_session ctx =
  match ctx.transaction with
  | None -> None
  | Some tx ->
      let start = not tx.started in
      tx.started <- true;
      let read_concern =
        if start then
          Option.bind tx.options (fun options ->
              Option.map
                (function
                  | Ent_ocaml.Read_local -> Mongo_command.Local
                  | Read_majority -> Mongo_command.Majority
                  | Read_linearizable -> Mongo_command.Linearizable
                  | Read_available -> Mongo_command.Available
                  | Read_snapshot -> Mongo_command.Snapshot
                  | Read_custom level -> Mongo_command.Custom level)
                options.read_concern)
        else None
      in
      Some
        (Mongo_session.transaction_context ?read_concern ~start tx.session
           ~txn_number:tx.txn_number)

let backend_error operation entity error =
  `Backend
    (Printf.sprintf "%s %s: %s" operation entity.Ent_ocaml.collection
       (Mongo_error.to_string error))

let bad_query message = Error (`Bad_query message)

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

let ordered_doc fields =
  List.fold_right
    (fun (name, value) acc -> Bson.add_element name value acc)
    fields Bson.empty

let find_edge (entity : Ent_ocaml.entity) name =
  List.find_opt (fun (edge : Ent_ocaml.edge) -> edge.name = name) entity.edges

let field_storage_key ?(missing = "field not found: ") (entity : Ent_ocaml.entity)
    name =
  match List.find_opt (fun (field : Ent_ocaml.field) -> field.name = name) entity.fields with
  | Some field -> Ok field.storage_key
  | None -> Error (`Bad_schema (missing ^ name))

let predicate_field ?entity field =
  match entity with
  | None -> Ok field
  | Some entity -> (
      match field_storage_key entity field with
      | Ok key -> Ok key
      | Error (`Bad_schema _) -> Ok field
      | Error _ as error -> error)

let split_dotted_field field =
  match String.split_on_char '.' field with
  | [] -> None
  | root :: path -> Some (root, path)

let json_path_field ?entity field path =
  let validate_path = function
    | [] -> Error (`Bad_query "json predicate path must not be empty")
    | segments ->
        if
          List.exists
            (fun segment ->
              segment = "" || String.contains segment '.'
              || String.starts_with ~prefix:"$" segment)
            segments
        then Error (`Bad_query "json predicate path contains invalid segment")
        else Ok (String.concat "." segments)
  in
  match (entity, validate_path path) with
  | _, (Error _ as error) -> error
  | None, Ok path -> Ok (field ^ "." ^ path)
  | Some (entity : Ent_ocaml.entity), Ok path -> (
      match
        List.find_opt
          (fun (candidate : Ent_ocaml.field) -> candidate.name = field)
          entity.fields
      with
      | None -> Error (`Bad_query ("field not found: " ^ field))
      | Some { typ = Ent_ocaml.Json; storage_key; _ } ->
          Ok (storage_key ^ "." ^ path)
      | Some _ -> Error (`Bad_query ("json predicate field is not json: " ^ field)))

let order_field ?entity field =
  match entity with
  | None -> field
  | Some entity -> (
      match field_storage_key entity field with
      | Ok key -> key
      | Error _ -> (
          match split_dotted_field field with
          | Some (root, (_ :: _ as path)) -> (
              match json_path_field ~entity root path with
              | Ok key -> key
              | Error _ -> field)
          | Some _ | None -> field))

let order_storage_key (entity : Ent_ocaml.entity) field =
  match field_storage_key entity field with
  | Ok key -> Ok key
  | Error (`Bad_schema _) -> (
      match split_dotted_field field with
      | Some (root, (_ :: _ as path)) -> json_path_field ~entity root path
      | Some _ | None -> Error (`Bad_query ("field not found: " ^ field)))
  | Error _ as error -> error

let edge_storage_key (entity : Ent_ocaml.entity) name =
  match find_edge entity name with
  | None -> Error (`Bad_query ("edge not found: " ^ name))
  | Some { storage_key = Some key; _ } -> Ok key
  | Some _ ->
      Error
        (`Bad_query
          ("edge " ^ name ^ " does not have a stored foreign-key field"))

let rec remap_id_predicate storage_key = function
  | Ent_ocaml.Eq ("id", value) -> Ok (Ent_ocaml.Eq (storage_key, value))
  | In ("id", values) -> Ok (In (storage_key, values))
  | And predicates ->
      remap_id_predicates storage_key predicates
      |> Result.map (fun predicates -> Ent_ocaml.And predicates)
  | Or predicates ->
      remap_id_predicates storage_key predicates
      |> Result.map (fun predicates -> Ent_ocaml.Or predicates)
  | Not predicate ->
      remap_id_predicate storage_key predicate
      |> Result.map (fun predicate -> Ent_ocaml.Not predicate)
  | _ ->
      Error
        (`Bad_query
          "stored edge predicates currently support target id equality or membership")

and remap_id_predicates storage_key predicates =
  let rec loop acc = function
    | [] -> Ok (List.rev acc)
    | predicate :: rest -> (
        match remap_id_predicate storage_key predicate with
        | Ok predicate -> loop (predicate :: acc) rest
        | Error _ as error -> error)
  in
  loop [] predicates

let target_field_path (entity : Ent_ocaml.entity) field =
  match order_storage_key entity field with
  | Ok key -> Ok key
  | Error _ as error -> error

let rec prefix_target_predicate (target : Ent_ocaml.entity) ~prefix = function
  | Ent_ocaml.Eq (field, value) ->
      target_field_path target field
      |> Result.map (fun field -> Ent_ocaml.Eq (prefix ^ "." ^ field, value))
  | Neq (field, value) ->
      target_field_path target field
      |> Result.map (fun field -> Ent_ocaml.Neq (prefix ^ "." ^ field, value))
  | Gt (field, value) ->
      target_field_path target field
      |> Result.map (fun field -> Ent_ocaml.Gt (prefix ^ "." ^ field, value))
  | Gte (field, value) ->
      target_field_path target field
      |> Result.map (fun field -> Ent_ocaml.Gte (prefix ^ "." ^ field, value))
  | Lt (field, value) ->
      target_field_path target field
      |> Result.map (fun field -> Ent_ocaml.Lt (prefix ^ "." ^ field, value))
  | Lte (field, value) ->
      target_field_path target field
      |> Result.map (fun field -> Ent_ocaml.Lte (prefix ^ "." ^ field, value))
  | In (field, values) ->
      target_field_path target field
      |> Result.map (fun field -> Ent_ocaml.In (prefix ^ "." ^ field, values))
  | Not_in (field, values) ->
      target_field_path target field
      |> Result.map (fun field -> Ent_ocaml.Not_in (prefix ^ "." ^ field, values))
  | Is_nil field ->
      target_field_path target field
      |> Result.map (fun field -> Ent_ocaml.Is_nil (prefix ^ "." ^ field))
  | Not_nil field ->
      target_field_path target field
      |> Result.map (fun field -> Ent_ocaml.Not_nil (prefix ^ "." ^ field))
  | Contains (field, value) ->
      target_field_path target field
      |> Result.map (fun field -> Ent_ocaml.Contains (prefix ^ "." ^ field, value))
  | Has_prefix (field, value) ->
      target_field_path target field
      |> Result.map (fun field -> Ent_ocaml.Has_prefix (prefix ^ "." ^ field, value))
  | Has_suffix (field, value) ->
      target_field_path target field
      |> Result.map (fun field -> Ent_ocaml.Has_suffix (prefix ^ "." ^ field, value))
  | Json_eq (field, path, value) ->
      json_path_field ~entity:target field path
      |> Result.map (fun field -> Ent_ocaml.Eq (prefix ^ "." ^ field, value))
  | Json_neq (field, path, value) ->
      json_path_field ~entity:target field path
      |> Result.map (fun field -> Ent_ocaml.Neq (prefix ^ "." ^ field, value))
  | Json_gt (field, path, value) ->
      json_path_field ~entity:target field path
      |> Result.map (fun field -> Ent_ocaml.Gt (prefix ^ "." ^ field, value))
  | Json_gte (field, path, value) ->
      json_path_field ~entity:target field path
      |> Result.map (fun field -> Ent_ocaml.Gte (prefix ^ "." ^ field, value))
  | Json_lt (field, path, value) ->
      json_path_field ~entity:target field path
      |> Result.map (fun field -> Ent_ocaml.Lt (prefix ^ "." ^ field, value))
  | Json_lte (field, path, value) ->
      json_path_field ~entity:target field path
      |> Result.map (fun field -> Ent_ocaml.Lte (prefix ^ "." ^ field, value))
  | Json_in (field, path, values) ->
      json_path_field ~entity:target field path
      |> Result.map (fun field -> Ent_ocaml.In (prefix ^ "." ^ field, values))
  | Json_not_in (field, path, values) ->
      json_path_field ~entity:target field path
      |> Result.map (fun field -> Ent_ocaml.Not_in (prefix ^ "." ^ field, values))
  | Json_is_nil (field, path) ->
      json_path_field ~entity:target field path
      |> Result.map (fun field -> Ent_ocaml.Is_nil (prefix ^ "." ^ field))
  | Json_not_nil (field, path) ->
      json_path_field ~entity:target field path
      |> Result.map (fun field -> Ent_ocaml.Not_nil (prefix ^ "." ^ field))
  | And predicates ->
      prefix_target_predicates target ~prefix predicates
      |> Result.map (fun predicates -> Ent_ocaml.And predicates)
  | Or predicates ->
      prefix_target_predicates target ~prefix predicates
      |> Result.map (fun predicates -> Ent_ocaml.Or predicates)
  | Not predicate ->
      prefix_target_predicate target ~prefix predicate
      |> Result.map (fun predicate -> Ent_ocaml.Not predicate)
  | Has_edge edge ->
      edge_storage_key target edge
      |> Result.map (fun storage_key -> Ent_ocaml.Not_nil (prefix ^ "." ^ storage_key))
  | Has_edge_with (edge, predicates) -> (
      match edge_storage_key target edge with
      | Error _ as error -> error
      | Ok storage_key ->
          remap_id_predicates (prefix ^ "." ^ storage_key) predicates
          |> Result.map (function
               | [] -> Ent_ocaml.Not_nil (prefix ^ "." ^ storage_key)
               | [ predicate ] -> predicate
               | predicates -> Ent_ocaml.And predicates))
  | Has_edge_with_target _ | Backend _ ->
      Error
        (`Bad_query
          "nested target edge predicates currently require a stored foreign-key id filter")

and prefix_target_predicates target ~prefix predicates =
  let rec loop acc = function
    | [] -> Ok (List.rev acc)
    | predicate :: rest -> (
        match prefix_target_predicate target ~prefix predicate with
        | Ok predicate -> loop (predicate :: acc) rest
        | Error _ as error -> error)
  in
  loop [] predicates

let rec predicate_to_bson ?entity predicate =
  match predicate with
  | Ent_ocaml.Eq (field, value) -> (
      match (predicate_field ?entity field, value_to_bson value) with
      | Ok field, Ok bson -> Ok (doc [ (field, bson) ])
      | Error _ as error, _ | _, (Error _ as error) -> error)
  | Neq (field, value) -> (
      match predicate_field ?entity field with
      | Ok field -> op_doc field "$ne" value
      | Error _ as error -> error)
  | Gt (field, value) -> (
      match predicate_field ?entity field with
      | Ok field -> op_doc field "$gt" value
      | Error _ as error -> error)
  | Gte (field, value) -> (
      match predicate_field ?entity field with
      | Ok field -> op_doc field "$gte" value
      | Error _ as error -> error)
  | Lt (field, value) -> (
      match predicate_field ?entity field with
      | Ok field -> op_doc field "$lt" value
      | Error _ as error -> error)
  | Lte (field, value) -> (
      match predicate_field ?entity field with
      | Ok field -> op_doc field "$lte" value
      | Error _ as error -> error)
  | In (field, values) -> (
      match predicate_field ?entity field with
      | Ok field -> op_doc field "$in" (V_list values)
      | Error _ as error -> error)
  | Not_in (field, values) -> (
      match predicate_field ?entity field with
      | Ok field -> op_doc field "$nin" (V_list values)
      | Error _ as error -> error)
  | Is_nil field -> (
      match predicate_field ?entity field with
      | Ok field -> Ok (doc [ (field, Bson.create_null ()) ])
      | Error _ as error -> error)
  | Not_nil field -> (
      match predicate_field ?entity field with
      | Ok field ->
          Ok
            (doc
               [
                 ( field,
                   Bson.create_doc_element
                     (doc [ ("$ne", Bson.create_null ()) ]) );
               ])
      | Error _ as error -> error)
  | Json_eq (field, path, value) -> (
      match (json_path_field ?entity field path, value_to_bson value) with
      | Ok field, Ok bson -> Ok (doc [ (field, bson) ])
      | Error _ as error, _ | _, (Error _ as error) -> error)
  | Json_neq (field, path, value) -> (
      match json_path_field ?entity field path with
      | Ok field -> op_doc field "$ne" value
      | Error _ as error -> error)
  | Json_gt (field, path, value) -> (
      match json_path_field ?entity field path with
      | Ok field -> op_doc field "$gt" value
      | Error _ as error -> error)
  | Json_gte (field, path, value) -> (
      match json_path_field ?entity field path with
      | Ok field -> op_doc field "$gte" value
      | Error _ as error -> error)
  | Json_lt (field, path, value) -> (
      match json_path_field ?entity field path with
      | Ok field -> op_doc field "$lt" value
      | Error _ as error -> error)
  | Json_lte (field, path, value) -> (
      match json_path_field ?entity field path with
      | Ok field -> op_doc field "$lte" value
      | Error _ as error -> error)
  | Json_in (field, path, values) -> (
      match json_path_field ?entity field path with
      | Ok field -> op_doc field "$in" (V_list values)
      | Error _ as error -> error)
  | Json_not_in (field, path, values) -> (
      match json_path_field ?entity field path with
      | Ok field -> op_doc field "$nin" (V_list values)
      | Error _ as error -> error)
  | Json_is_nil (field, path) -> (
      match json_path_field ?entity field path with
      | Ok field -> Ok (doc [ (field, Bson.create_null ()) ])
      | Error _ as error -> error)
  | Json_not_nil (field, path) -> (
      match json_path_field ?entity field path with
      | Ok field ->
          Ok
            (doc
               [
                 ( field,
                   Bson.create_doc_element
                     (doc [ ("$ne", Bson.create_null ()) ]) );
               ])
      | Error _ as error -> error)
  | And predicates -> logical ?entity "$and" predicates
  | Or predicates -> logical ?entity "$or" predicates
  | Not predicate -> (
      match predicate_to_bson ?entity predicate with
      | Ok bson -> Ok (doc [ ("$nor", Bson.create_list [ Bson.create_doc_element bson ]) ])
      | Error _ as error -> error)
  | Contains (field, value) -> (
      match predicate_field ?entity field with
      | Ok field ->
          Ok
            (doc
               [
                 ( field,
                   Bson.create_doc_element
                     (doc [ ("$regex", Bson.create_string (".*" ^ value ^ ".*")) ])
                 );
               ])
      | Error _ as error -> error)
  | Has_prefix (field, value) -> (
      match predicate_field ?entity field with
      | Ok field ->
          Ok
            (doc
               [
                 ( field,
                   Bson.create_doc_element
                     (doc [ ("$regex", Bson.create_string ("^" ^ value)) ]) );
               ])
      | Error _ as error -> error)
  | Has_suffix (field, value) -> (
      match predicate_field ?entity field with
      | Ok field ->
          Ok
            (doc
               [
                 ( field,
                   Bson.create_doc_element
                     (doc [ ("$regex", Bson.create_string (value ^ "$")) ]) );
               ])
      | Error _ as error -> error)
  | Has_edge edge_name -> (
      match entity with
      | None -> Error (`Bad_query "edge predicate needs entity context")
      | Some entity -> (
          match edge_storage_key entity edge_name with
          | Error _ as error -> error
          | Ok storage_key ->
              Ok
                (doc
                   [
                     ( storage_key,
                       Bson.create_doc_element
                         (doc
                            [
                              ("$exists", Bson.create_boolean true);
                              ("$ne", Bson.create_null ());
                            ]) );
                   ])))
  | Has_edge_with (edge_name, predicates) -> (
      match entity with
      | None -> Error (`Bad_query "edge predicate needs entity context")
      | Some entity -> (
          match edge_storage_key entity edge_name with
          | Error _ as error -> error
          | Ok storage_key -> (
              match remap_id_predicates storage_key predicates with
              | Error _ as error -> error
              | Ok [] -> predicate_to_bson ~entity (Has_edge edge_name)
              | Ok [ predicate ] -> predicate_to_bson ~entity predicate
              | Ok predicates -> predicate_to_bson ~entity (And predicates))))
  | Has_edge_with_target _ ->
      Error
        (`Bad_query
          "target edge predicates require aggregate find planning")
  | Backend (_, _) ->
      Error (`Bad_query "backend predicates need typed backend-specific support")

and logical ?entity op predicates =
  let rec loop acc = function
    | [] -> Ok (doc [ (op, Bson.create_list (List.rev acc)) ])
    | predicate :: rest -> (
        match predicate_to_bson ?entity predicate with
        | Ok bson -> loop (Bson.create_doc_element bson :: acc) rest
        | Error _ as error -> error)
  in
  loop [] predicates

let filter_to_bson (query : Ent_ocaml.query) =
  match query.Ent_ocaml.predicates with
  | [] -> Ok Bson.empty
  | [ predicate ] -> predicate_to_bson ~entity:query.entity predicate
  | predicates -> predicate_to_bson ~entity:query.entity (And predicates)

let sort_to_bson ?entity (orders : Ent_ocaml.order list) =
  let direction = function
    | Ent_ocaml.Asc -> Bson.create_int32 1l
    | Desc -> Bson.create_int32 (-1l)
  in
  let order_field_for_sort order =
    let ({ Ent_ocaml.target = order_target; _ } : Ent_ocaml.order) = order in
    match order_target with
    | Ent_ocaml.Field_order field -> order_field ?entity field
    | Edge_field_order { edge; field; _ } -> edge ^ "." ^ field
    | Edge_count_order { edge; _ } -> edge ^ ".count"
    | Backend_order { backend; name; _ } -> backend ^ "." ^ name
  in
  match orders with
  | [] -> None
  | orders ->
      let fields =
        List.map
          (fun (order : Ent_ocaml.order) ->
            let field = order_field_for_sort order in
            (field, direction order.direction))
          orders
	      in
	      Some (ordered_doc fields)

let needs_aggregate_order (query : Ent_ocaml.query) =
  List.exists
    (fun (order : Ent_ocaml.order) ->
      let ({ Ent_ocaml.target = order_target; _ } : Ent_ocaml.order) = order in
      match order_target with
      | Ent_ocaml.Edge_field_order _ -> true
      | Ent_ocaml.Edge_count_order _ -> true
      | Ent_ocaml.Backend_order _ -> true
      | Ent_ocaml.Field_order _ -> false)
    query.orders

let edge_order_temp index = "__ent_edge_order_" ^ string_of_int index
let backend_order_temp index = "__ent_backend_order_" ^ string_of_int index
let edge_predicate_temp index = "__ent_edge_pred_" ^ string_of_int index
let join_predicate_temp index = "__ent_join_pred_" ^ string_of_int index
let join_target_temp index = "__ent_join_target_" ^ string_of_int index
let nested_edge_target_temp index depth =
  "__ent_nested_edge_target_" ^ string_of_int index ^ "_" ^ string_of_int depth

let dedup_root_temp index = "__ent_dedup_root_" ^ string_of_int index

let find_join_edge (entity : Ent_ocaml.entity) edge_name =
  match find_edge entity edge_name with
  | None -> Ok None
  | Some { Ent_ocaml.join = None; _ } -> Ok None
  | Some ({ direction = From _; _ } : Ent_ocaml.edge) ->
      Error (`Bad_query "join edge predicates currently support to-edges")
  | Some { cardinality = One; _ } ->
      Error (`Bad_query "join edge predicates currently support to-many edges")
  | Some ({ join = Some _; _ } as edge) -> Ok (Some edge)

let join_edge_predicate_stage (query : Ent_ocaml.query) index predicate =
  let edge_name =
    match predicate with
    | Ent_ocaml.Has_edge edge_name | Has_edge_with (edge_name, _) -> edge_name
    | _ -> ""
  in
  match
    ( find_join_edge query.entity edge_name,
      field_storage_key query.entity "id" )
  with
  | Error _ as error, _ | _, (Error _ as error) -> error
  | Ok None, _ ->
      Error (`Bad_query ("edge " ^ edge_name ^ " is not a join-backed edge"))
  | Ok (Some { join = None; _ }), _ ->
      Error (`Bad_query ("edge " ^ edge_name ^ " is not a join-backed edge"))
  | Ok (Some { join = Some join; _ }), Ok source_id_key -> (
      let temp = join_predicate_temp index in
      let lookup =
        doc
          [
            ("from", Bson.create_string join.collection);
            ("localField", Bson.create_string source_id_key);
            ("foreignField", Bson.create_string join.source_key);
            ("as", Bson.create_string temp);
          ]
      in
      let match_filter =
        match predicate with
        | Ent_ocaml.Has_edge _ ->
            Ok
              (doc
                 [
                   ( temp ^ ".0",
                     Bson.create_doc_element
                       (doc [ ("$exists", Bson.create_boolean true) ]) );
                 ])
        | Has_edge_with (_, predicates) -> (
            match remap_id_predicates (temp ^ "." ^ join.target_key) predicates with
            | Error _ as error -> error
            | Ok [] ->
                Ok
                  (doc
                     [
                       ( temp ^ ".0",
                         Bson.create_doc_element
                           (doc [ ("$exists", Bson.create_boolean true) ]) );
                     ])
            | Ok [ predicate ] -> predicate_to_bson predicate
            | Ok predicates -> predicate_to_bson (Ent_ocaml.And predicates))
        | _ -> Error (`Bad_query "expected join edge predicate")
      in
      match match_filter with
      | Error _ as error -> error
      | Ok filter ->
          Ok
            [
              doc [ ("$lookup", Bson.create_doc_element lookup) ];
              doc [ ("$match", Bson.create_doc_element filter) ];
            ])

let rec predicate_has_join_edge entity = function
  | Ent_ocaml.Has_edge edge | Has_edge_with (edge, _) -> (
      match find_join_edge entity edge with
      | Ok (Some _) -> true
      | Ok None -> false
      | Error _ -> true)
  | And predicates | Or predicates ->
      List.exists (predicate_has_join_edge entity) predicates
  | Not predicate -> predicate_has_join_edge entity predicate
  | Eq _ | Neq _ | Gt _ | Gte _ | Lt _ | Lte _ | In _ | Not_in _ | Is_nil _
  | Not_nil _ | Contains _ | Has_prefix _ | Has_suffix _ | Json_eq _ | Json_neq _
  | Json_gt _ | Json_gte _ | Json_lt _ | Json_lte _ | Json_in _ | Json_not_in _
  | Json_is_nil _ | Json_not_nil _ | Has_edge_with_target _ | Backend _ ->
      false

let partition_join_edge_predicates (query : Ent_ocaml.query) predicates =
  let rec loop join normal = function
    | [] -> Ok (List.rev join, List.rev normal)
    | (Ent_ocaml.Has_edge edge_name as predicate) :: rest
    | (Has_edge_with (edge_name, _) as predicate) :: rest -> (
        match find_join_edge query.entity edge_name with
        | Error _ as error -> error
        | Ok (Some _) -> loop (predicate :: join) normal rest
        | Ok None -> loop join (predicate :: normal) rest)
    | predicate :: rest when predicate_has_join_edge query.entity predicate ->
        Error (`Bad_query "join edge predicates must be top-level query predicates")
    | predicate :: rest -> loop join (predicate :: normal) rest
  in
  loop [] [] predicates

let join_edge_predicate_lookup_pipeline query join_predicates =
  let rec loop index acc = function
    | [] -> Ok (List.rev acc)
    | predicate :: rest -> (
        match join_edge_predicate_stage query index predicate with
        | Ok stages -> loop (index + 1) (List.rev_append stages acc) rest
        | Error _ as error -> error)
  in
  loop 0 [] join_predicates

let partition_nested_target_predicates predicates =
  let rec loop nested normal = function
    | [] -> (List.rev nested, List.rev normal)
    | Ent_ocaml.Has_edge_with_target { edge; target; predicates } :: rest ->
        loop ((edge, target, predicates) :: nested) normal rest
    | predicate :: rest -> loop nested (predicate :: normal) rest
  in
  loop [] [] predicates

let match_prefixed_target_predicates target ~prefix predicates =
  match prefix_target_predicates target ~prefix predicates with
  | Error _ as error -> error
  | Ok [] -> Ok []
  | Ok [ predicate ] -> (
      match predicate_to_bson predicate with
      | Error _ as error -> error
      | Ok filter -> Ok [ doc [ ("$match", Bson.create_doc_element filter) ] ])
  | Ok predicates -> (
      match predicate_to_bson (Ent_ocaml.And predicates) with
      | Error _ as error -> error
      | Ok filter -> Ok [ doc [ ("$match", Bson.create_doc_element filter) ] ])

let rec nested_target_predicate_stages index depth target ~prefix predicates =
  let nested, normal = partition_nested_target_predicates predicates in
  match match_prefixed_target_predicates target ~prefix normal with
  | Error _ as error -> error
  | Ok normal_stages -> (
      let rec loop depth acc = function
        | [] -> Ok (normal_stages @ List.rev acc)
        | (edge, nested_target, predicates) :: rest -> (
            match
              nested_edge_target_lookup_stage index depth ~source_prefix:prefix
                ~source_entity:target ~edge ~target:nested_target ~predicates
            with
            | Ok stages -> loop (depth + 1) (List.rev_append stages acc) rest
            | Error _ as error -> error)
      in
      loop depth [] nested)

and nested_edge_target_lookup_stage index depth ~source_prefix ~source_entity
    ~edge ~target ~predicates =
  match find_edge source_entity edge with
  | None -> Error (`Bad_query ("edge not found: " ^ edge))
  | Some edge_desc when edge_desc.target <> target.Ent_ocaml.name ->
      Error
        (`Bad_query
          (Printf.sprintf "edge %s targets %s, not %s" edge_desc.name
             edge_desc.target target.name))
  | Some { direction = From _; _ } ->
      Error (`Bad_query "nested target edge predicates currently support to-edges")
  | Some edge_desc -> (
      match
        ( edge_desc.cardinality,
          edge_desc.storage_key,
          edge_desc.join,
          field_storage_key target "id" )
      with
      | Ent_ocaml.One, Some local_key, None, Ok foreign_key -> (
          let temp = nested_edge_target_temp index depth in
          let lookup =
            doc
              [
                ("from", Bson.create_string target.collection);
                ("localField", Bson.create_string (source_prefix ^ "." ^ local_key));
                ("foreignField", Bson.create_string foreign_key);
                ("as", Bson.create_string temp);
              ]
          in
          let unwind = doc [ ("path", Bson.create_string ("$" ^ temp)) ] in
          match
            nested_target_predicate_stages index (depth + 1) target ~prefix:temp
              predicates
          with
          | Error _ as error -> error
          | Ok stages ->
              Ok
                (doc [ ("$lookup", Bson.create_doc_element lookup) ]
                :: doc [ ("$unwind", Bson.create_doc_element unwind) ]
                :: stages))
      | Ent_ocaml.Many, _, _, _ ->
          Error
            (`Bad_query
              "nested target edge predicates currently support to-one stored-FK edges")
      | Ent_ocaml.One, _, Some _, _ ->
          Error
            (`Bad_query
              ("edge " ^ edge_desc.name ^ " has join metadata but is not to-one"))
      | Ent_ocaml.One, None, None, _ ->
          Error
            (`Bad_query
              ("edge " ^ edge_desc.name
             ^ " does not have a stored foreign-key field"))
      | _, _, _, (Error _ as error) -> error)

let edge_target_predicate_lookup_stage (query : Ent_ocaml.query) index ~edge
    ~target ~predicates =
  match find_edge query.Ent_ocaml.entity edge with
  | None -> Error (`Bad_query ("edge not found: " ^ edge))
  | Some edge_desc when edge_desc.target <> target.Ent_ocaml.name ->
      Error
        (`Bad_query
          (Printf.sprintf "edge %s targets %s, not %s" edge_desc.name
             edge_desc.target target.name))
  | Some { direction = From _; _ } ->
      Error (`Bad_query "target edge predicates currently support to-edges")
  | Some edge_desc -> (
      match
        ( edge_desc.cardinality,
          edge_desc.storage_key,
          edge_desc.join,
          field_storage_key target "id",
          field_storage_key query.entity "id" )
      with
      | Ent_ocaml.One, Some local_key, None, Ok foreign_key, _ -> (
          let temp = edge_predicate_temp index in
          let lookup =
            doc
              [
                ("from", Bson.create_string target.collection);
                ("localField", Bson.create_string local_key);
                ("foreignField", Bson.create_string foreign_key);
                ("as", Bson.create_string temp);
              ]
          in
          let unwind = doc [ ("path", Bson.create_string ("$" ^ temp)) ] in
          if predicates = [] then
            Ok [ doc [ ("$lookup", Bson.create_doc_element lookup) ] ]
          else
            match nested_target_predicate_stages index 0 target ~prefix:temp predicates with
            | Error _ as error -> error
            | Ok stages ->
                Ok
                  (doc [ ("$lookup", Bson.create_doc_element lookup) ]
                  :: doc [ ("$unwind", Bson.create_doc_element unwind) ]
                  :: stages))
      | Many, None, Some join, Ok foreign_key, Ok source_id_key -> (
          let join_temp = join_predicate_temp index in
          let target_temp = join_target_temp index in
          let root_temp = dedup_root_temp index in
          let join_lookup =
            doc
              [
                ("from", Bson.create_string join.collection);
                ("localField", Bson.create_string source_id_key);
                ("foreignField", Bson.create_string join.source_key);
                ("as", Bson.create_string join_temp);
              ]
          in
          let target_lookup =
            doc
              [
                ("from", Bson.create_string target.collection);
                ( "localField",
                  Bson.create_string (join_temp ^ "." ^ join.target_key) );
                ("foreignField", Bson.create_string foreign_key);
                ("as", Bson.create_string target_temp);
              ]
          in
          let unwind_join =
            doc [ ("path", Bson.create_string ("$" ^ join_temp)) ]
          in
          let unwind_target =
            doc [ ("path", Bson.create_string ("$" ^ target_temp)) ]
          in
          let group =
            doc
              [
                ("_id", Bson.create_string ("$" ^ source_id_key));
                ( root_temp,
                  Bson.create_doc_element
                    (doc [ ("$first", Bson.create_string "$$ROOT") ]) );
              ]
          in
          let replace_root =
            doc [ ("newRoot", Bson.create_string ("$" ^ root_temp)) ]
          in
          let base_stages =
            [
              doc [ ("$lookup", Bson.create_doc_element join_lookup) ];
              doc [ ("$unwind", Bson.create_doc_element unwind_join) ];
              doc [ ("$lookup", Bson.create_doc_element target_lookup) ];
              doc [ ("$unwind", Bson.create_doc_element unwind_target) ];
            ]
          in
          let dedup_stages =
            [
              doc [ ("$group", Bson.create_doc_element group) ];
              doc [ ("$replaceRoot", Bson.create_doc_element replace_root) ];
            ]
          in
          match
            nested_target_predicate_stages index 0 target ~prefix:target_temp
              predicates
          with
          | Error _ as error -> error
          | Ok stages -> Ok (base_stages @ stages @ dedup_stages))
      | Many, Some _, Some _, _, _ ->
          Error
            (`Bad_query
              ("edge " ^ edge_desc.name
             ^ " must use either storage_key or join metadata, not both"))
      | Many, Some _, None, _, _ ->
          Error (`Bad_query "target edge predicates currently support to-one stored-FK edges")
      | One, _, Some _, _, _ ->
          Error
            (`Bad_query
              ("edge " ^ edge_desc.name ^ " has join metadata but is not to-many"))
      | _, None, None, _, _ ->
          Error
            (`Bad_query
              ("edge " ^ edge_desc.name
             ^ " does not have a stored foreign-key field"))
      | _, _, _, (Error _ as error), _
      | _, _, _, _, (Error _ as error) ->
          error)

let rec predicate_has_edge_target = function
  | Ent_ocaml.Has_edge_with_target _ -> true
  | And predicates | Or predicates -> List.exists predicate_has_edge_target predicates
  | Not predicate -> predicate_has_edge_target predicate
  | Eq _ | Neq _ | Gt _ | Gte _ | Lt _ | Lte _ | In _ | Not_in _ | Is_nil _
  | Not_nil _ | Contains _ | Has_prefix _ | Has_suffix _ | Json_eq _ | Json_neq _
  | Json_gt _ | Json_gte _ | Json_lt _ | Json_lte _ | Json_in _ | Json_not_in _
  | Json_is_nil _ | Json_not_nil _ | Has_edge _ | Has_edge_with _ | Backend _ ->
      false

let partition_edge_target_predicates (predicates : Ent_ocaml.predicate list) =
  let rec loop edge_target normal = function
    | [] -> Ok (List.rev edge_target, List.rev normal)
    | Ent_ocaml.Has_edge_with_target { edge; target; predicates } :: rest ->
        loop ((edge, target, predicates) :: edge_target) normal rest
    | predicate :: rest when predicate_has_edge_target predicate ->
        Error
          (`Bad_query
            "target edge predicates must be top-level query predicates")
    | predicate :: rest -> loop edge_target (predicate :: normal) rest
  in
  loop [] [] predicates

let edge_target_predicate_lookup_pipeline query edge_predicates =
  let rec loop index acc = function
    | [] -> Ok (List.rev acc)
    | (edge, target, predicates) :: rest -> (
        match
          edge_target_predicate_lookup_stage query index ~edge ~target
            ~predicates
        with
        | Ok stages -> loop (index + 1) (List.rev_append stages acc) rest
        | Error _ as error -> error)
  in
  loop 0 [] edge_predicates

let has_edge_target_predicate (query : Ent_ocaml.query) =
  List.exists predicate_has_edge_target query.Ent_ocaml.predicates

let has_join_edge_predicate (query : Ent_ocaml.query) =
  List.exists
    (predicate_has_join_edge query.Ent_ocaml.entity)
    query.Ent_ocaml.predicates

let edge_order_field (query : Ent_ocaml.query) index (order : Ent_ocaml.order) =
  let ({ Ent_ocaml.target = order_target; _ } : Ent_ocaml.order) = order in
  match order_target with
  | Ent_ocaml.Field_order field -> order_storage_key query.Ent_ocaml.entity field
  | Edge_field_order { edge; target; field } -> (
      match find_edge query.Ent_ocaml.entity edge with
      | None -> Error (`Bad_query ("edge not found: " ^ edge))
      | Some edge_desc when edge_desc.target <> target.name ->
          Error
            (`Bad_query
              (Printf.sprintf "edge %s targets %s, not %s" edge_desc.name
                 edge_desc.target target.name))
      | Some { direction = From _; _ } ->
          Error (`Bad_query "edge-field ordering currently supports to-edges")
      | Some { cardinality = Many; _ } ->
          Error (`Bad_query "edge-field ordering currently supports to-one edges")
      | Some edge_desc -> (
          match (edge_desc.storage_key, field_storage_key target field) with
          | Some _, Ok target_field_key ->
              Ok (edge_order_temp index ^ "." ^ target_field_key)
          | None, _ ->
              Error
                (`Bad_query
                  ("edge " ^ edge_desc.name
                 ^ " does not have a stored foreign-key field"))
          | _, (Error _ as error) -> error))
  | Edge_count_order { edge; target } -> (
      match find_edge query.Ent_ocaml.entity edge with
      | None -> Error (`Bad_query ("edge not found: " ^ edge))
      | Some edge_desc when edge_desc.target <> target.name ->
          Error
            (`Bad_query
              (Printf.sprintf "edge %s targets %s, not %s" edge_desc.name
                 edge_desc.target target.name))
      | Some { direction = From _; _ } ->
          Error (`Bad_query "edge-count ordering currently supports to-edges")
      | Some { cardinality = One; _ } ->
          Error
            (`Bad_query "edge-count ordering currently supports to-many edges")
      | Some edge_desc -> (
          match (edge_desc.storage_key, edge_desc.join) with
          | Some _, None | None, Some _ -> Ok (edge_order_temp index ^ "_count")
          | Some _, Some _ ->
              Error
                (`Bad_query
                  ("edge " ^ edge_desc.name
                 ^ " must use either storage_key or join metadata, not both"))
          | None, None ->
              Error
                (`Bad_query
                  ("edge " ^ edge_desc.name
                 ^ " does not have a stored foreign-key field"))))
  | Backend_order { backend; _ } ->
      if backend = "mongo" then Ok (backend_order_temp index)
      else Error (`Bad_query ("unsupported backend order: " ^ backend))

let sort_to_bson_result (query : Ent_ocaml.query) =
  let direction = function
    | Ent_ocaml.Asc -> Bson.create_int32 1l
    | Desc -> Bson.create_int32 (-1l)
  in
  let rec loop index acc = function
    | [] -> Ok (if acc = [] then None else Some (ordered_doc (List.rev acc)))
    | (order : Ent_ocaml.order) :: rest -> (
        match edge_order_field query index order with
        | Ok field ->
            loop (index + 1) ((field, direction order.direction) :: acc) rest
        | Error _ as error -> error)
  in
  loop 0 [] query.orders

let projection_to_bson (query : Ent_ocaml.query) =
  let add_projection acc key =
    if List.exists (fun (existing, _) -> existing = key) acc then acc
    else (key, Bson.create_int32 1l) :: acc
  in
  let rec add_select acc = function
    | [] -> Ok acc
    | field :: rest -> (
        match field_storage_key query.entity field with
        | Ok key -> add_select (add_projection acc key) rest
        | Error _ as error -> error)
  in
  let rec add_order_values acc = function
    | [] -> Ok acc
    | ({ value_alias = None; _ } : Ent_ocaml.order) :: rest ->
        add_order_values acc rest
    | ({ target = order_target; value_alias = Some alias; _ } : Ent_ocaml.order) :: rest -> (
        let field_key =
          match order_target with
          | Ent_ocaml.Field_order field -> order_storage_key query.entity field
          | Edge_field_order _ | Edge_count_order _ | Backend_order _ -> Ok alias
        in
        match field_key with
        | Ok key -> add_order_values (add_projection acc key) rest
        | Error _ as error -> error)
  in
  match add_select [] query.select with
  | Error _ as error -> error
  | Ok fields -> (
      match add_order_values fields query.orders with
      | Error _ as error -> error
      | Ok [] -> Ok None
      | Ok fields -> Ok (Some (doc (List.rev fields))))

let find_options (query : Ent_ocaml.query) =
  match (filter_to_bson query, projection_to_bson query) with
  | Error _ as error, _ | _, (Error _ as error) -> error
  | Ok filter, Ok projection ->
      Ok
         {
           (Mongo_crud.default_find query.Ent_ocaml.entity.collection filter) with
           projection;
           sort = sort_to_bson ~entity:query.entity query.orders;
           skip = query.offset;
           limit = query.limit;
         }

let aggregate_expr op storage_key =
  let field_path = Bson.create_string ("$" ^ storage_key) in
  match op with
  | Ent_ocaml.Count -> Bson.create_doc_element (doc [ ("$sum", Bson.create_int32 1l) ])
  | Min _ -> Bson.create_doc_element (doc [ ("$min", field_path) ])
  | Max _ -> Bson.create_doc_element (doc [ ("$max", field_path) ])
  | Sum _ -> Bson.create_doc_element (doc [ ("$sum", field_path) ])
  | Avg _ -> Bson.create_doc_element (doc [ ("$avg", field_path) ])

let aggregate_field = function
  | Ent_ocaml.Count -> None
  | Min field | Max field | Sum field | Avg field -> Some field

let aggregate_pipeline ~group_key (aggregate : Ent_ocaml.aggregate) =
  let query = aggregate.query in
  let field_key =
    match aggregate_field aggregate.op with
    | None -> Ok ""
    | Some field -> field_storage_key query.entity field
  in
  match (filter_to_bson query, field_key, group_key) with
  | Error _ as error, _, _ | _, (Error _ as error), _ | _, _, (Error _ as error) ->
      error
  | Ok filter, Ok storage_key, Ok group_key ->
      let match_stage =
        if filter = Bson.empty then []
        else [ doc [ ("$match", Bson.create_doc_element filter) ] ]
      in
      let group =
        doc
          [
            ( "_id",
              match group_key with
              | None -> Bson.create_null ()
              | Some key -> Bson.create_string ("$" ^ key) );
            ("value", aggregate_expr aggregate.op storage_key);
          ]
      in
      Ok (match_stage @ [ doc [ ("$group", Bson.create_doc_element group) ] ])

let aggregate_pipeline_to_bson aggregate =
  aggregate_pipeline ~group_key:(Ok None) aggregate

let group_pipeline_to_bson (group : Ent_ocaml.group_aggregate) =
  let query = group.aggregate.query in
  aggregate_pipeline
    ~group_key:
      (field_storage_key query.entity group.group
      |> Result.map (fun key -> Some key))
    group.aggregate

let aggregate_scan_pipeline_to_bson (scan : Ent_ocaml.aggregate_scan) =
  let query = scan.query in
  let scan_expr (name, op) =
    let field_key =
      match aggregate_field op with
      | None -> Ok ""
      | Some field -> field_storage_key query.entity field
    in
    field_key |> Result.map (fun key -> (name, aggregate_expr op key))
  in
  let rec exprs acc = function
    | [] -> Ok (List.rev acc)
    | op :: rest -> (
        match scan_expr op with
        | Ok expr -> exprs (expr :: acc) rest
        | Error _ as error -> error)
  in
  match (filter_to_bson query, exprs [] scan.ops) with
  | Error _ as error, _ | _, (Error _ as error) -> error
  | Ok filter, Ok exprs ->
      let match_stage =
        if filter = Bson.empty then []
        else [ doc [ ("$match", Bson.create_doc_element filter) ] ]
      in
      let group =
        Bson.add_element "_id" (Bson.create_null ()) (doc exprs)
      in
      Ok (match_stage @ [ doc [ ("$group", Bson.create_doc_element group) ] ])

let index_storage_fields (entity : Ent_ocaml.entity) (index : Ent_ocaml.index) =
  let rec loop acc = function
    | [] -> Ok (List.rev acc)
    | field :: rest -> (
        match field_storage_key ~missing:"index field not found: " entity field with
        | Ok key -> loop (key :: acc) rest
        | Error _ as error -> error)
  in
  loop [] index.fields

let index_key_bson fields =
  let key =
    List.fold_right
      (fun field acc -> Bson.add_element field (Bson.create_int32 1l) acc)
      fields Bson.empty
  in
  Bson.add_element "key" (Bson.create_doc_element key) Bson.empty

let default_index_name key_bson =
  Bson.get_element "key" key_bson |> Bson.get_doc_element |> Bson.all_elements
  |> List.fold_left
       (fun name (key, element) ->
         let direction = Bson.get_int32 element in
         if name = "" then Printf.sprintf "%s_%ld" key direction
         else Printf.sprintf "%s_%s_%ld" name key direction)
       ""

let index_to_bson (entity : Ent_ocaml.entity) (index : Ent_ocaml.index) =
  match index_storage_fields entity index with
  | Error _ as error -> error
  | Ok [] -> Error (`Bad_schema "index has no fields")
  | Ok fields -> (
      let key_bson = index_key_bson fields in
      let name = Option.value index.name ~default:(default_index_name key_bson) in
      let base =
        key_bson
        |> Bson.add_element "name" (Bson.create_string name)
        |> Bson.add_element "v" (Bson.create_int32 1l)
      in
      let base =
        if index.unique then
          Bson.add_element "unique" (Bson.create_boolean true) base
        else base
      in
      match index.partial_filter with
      | [] -> Ok base
      | [ predicate ] -> (
          match predicate_to_bson ~entity predicate with
          | Ok filter ->
              Ok
                (Bson.add_element "partialFilterExpression"
                   (Bson.create_doc_element filter) base)
          | Error _ as error -> error)
      | predicates -> (
          match predicate_to_bson ~entity (Ent_ocaml.And predicates) with
          | Ok filter ->
              Ok
                (Bson.add_element "partialFilterExpression"
                   (Bson.create_doc_element filter) base)
          | Error _ as error -> error))

let ensure_index ctx (entity : Ent_ocaml.entity) (index : Ent_ocaml.index) =
  match index_to_bson entity index with
  | Error _ as error -> error
  | Ok index_bson ->
      (match
         Mongo_eio.direct_run_command ctx.client ctx.config.database
           [
             ("createIndexes", Bson.create_string entity.collection);
             ("indexes", Bson.create_doc_element_list [ index_bson ]);
           ]
       with
      | Ok _ -> Ok ()
      | Error error -> Error (backend_error "ensure_index" entity error))

let ensure_indexes ctx entities =
  let rec entity_loop = function
    | [] -> Ok ()
    | entity :: rest -> (
        let rec index_loop = function
          | [] -> entity_loop rest
          | index :: indexes -> (
              match ensure_index ctx entity index with
              | Ok () -> index_loop indexes
              | Error _ as error -> error)
        in
        index_loop entity.Ent_ocaml.indexes)
  in
  entity_loop entities

let index_check_ok check =
  match check.status with
  | Present -> true
  | Missing | Mismatched _ -> false

let index_check_to_string check =
  match check.status with
  | Present ->
      Printf.sprintf "%s.%s present" check.collection check.name
  | Missing ->
      Printf.sprintf "%s.%s missing" check.collection check.name
  | Mismatched reasons ->
      Printf.sprintf "%s.%s mismatched: %s" check.collection check.name
        (String.concat "; " reasons)

let index_actuals ctx (entity : Ent_ocaml.entity) =
  match
    Mongo_eio.direct_run_command ctx.client ctx.config.database
      [ ("listIndexes", Bson.create_string entity.collection) ]
  with
  | Error error -> Error (backend_error "list_indexes" entity error)
  | Ok response -> Ok (Mongo_command.cursor_batch response.Mongo_command.body)

let index_name bson = Bson.get_string (Bson.get_element "name" bson)

let index_doc name bson =
  try Some (Bson.get_doc_element (Bson.get_element name bson)) with
  | Not_found | Bson.Wrong_bson_type -> None

let index_bool ~default name bson =
  try Bson.get_boolean (Bson.get_element name bson) with
  | Not_found | Bson.Wrong_bson_type -> default

let doc_equal left right = Bson.to_simple_json left = Bson.to_simple_json right

let compare_index_bson expected actual =
  let mismatches = ref [] in
  let check name condition =
    if not condition then mismatches := name :: !mismatches
  in
  let expected_key = index_doc "key" expected in
  let actual_key = index_doc "key" actual in
  check "key"
    (match (expected_key, actual_key) with
    | Some expected, Some actual -> doc_equal expected actual
    | None, None -> true
    | Some _, None | None, Some _ -> false);
  check "unique"
    (index_bool ~default:false "unique" expected
    = index_bool ~default:false "unique" actual);
  let expected_partial = index_doc "partialFilterExpression" expected in
  let actual_partial = index_doc "partialFilterExpression" actual in
  check "partialFilterExpression"
    (match (expected_partial, actual_partial) with
    | Some expected, Some actual -> doc_equal expected actual
    | None, None -> true
    | Some _, None | None, Some _ -> false);
  List.rev !mismatches

let check_entity_indexes ctx (entity : Ent_ocaml.entity) =
  match index_actuals ctx entity with
  | Error _ as error -> error
  | Ok actuals ->
      let rec loop acc = function
        | [] -> Ok (List.rev acc)
        | index :: indexes -> (
            match index_to_bson entity index with
            | Error _ as error -> error
            | Ok expected ->
                let name = index_name expected in
                let status =
                  match List.find_opt (fun actual -> index_name actual = name) actuals with
                  | None -> Missing
                  | Some actual -> (
                      match compare_index_bson expected actual with
                      | [] -> Present
                      | mismatches -> Mismatched mismatches)
                in
                loop
                  ({
                     entity = entity.name;
                     collection = entity.collection;
                     name;
                     status;
                   }
                  :: acc)
                  indexes)
      in
      loop [] entity.indexes

let check_indexes ctx entities =
  let rec loop acc = function
    | [] -> Ok (List.rev acc)
    | entity :: rest -> (
        match check_entity_indexes ctx entity with
        | Error _ as error -> error
        | Ok checks -> loop (List.rev_append checks acc) rest)
  in
  loop [] entities

let verify_indexes ctx entities =
  match check_indexes ctx entities with
  | Error _ as error -> error
  | Ok checks -> (
      match List.filter (fun check -> not (index_check_ok check)) checks with
      | [] -> Ok ()
      | failed ->
          Error
            (`Bad_schema
              ("Mongo index drift: "
              ^ String.concat ", " (List.map index_check_to_string failed))))

let bson_type_element = function
  | [] -> Bson.create_list []
  | [ typ ] -> Bson.create_string typ
  | types -> Bson.create_list (List.map Bson.create_string types)

let rec json_schema_parts = function
  | Ent_ocaml.Option typ ->
      Option.map
        (fun (types, fields) -> ("null" :: types, fields))
        (json_schema_parts typ)
  | String | Uuid | Enum _ -> Some ([ "string" ], [])
  | Int | Int64 | Time_ms -> Some ([ "long" ], [])
  | Int32 -> Some ([ "int" ], [])
  | Float -> Some ([ "double" ], [])
  | Bool -> Some ([ "bool" ], [])
  | Bytes -> Some ([ "binData" ], [])
  | Custom _ -> Some ([ "object" ], [])
  | Json -> None
  | List typ ->
      let item_schema =
        match json_schema_parts typ with
        | None -> []
        | Some (types, fields) ->
            [
              ( "items",
                Bson.create_doc_element
                  (doc (("bsonType", bson_type_element types) :: fields)) );
            ]
      in
      Some ([ "array" ], item_schema)

let field_json_schema (field : Ent_ocaml.field) =
  match json_schema_parts field.typ with
  | None -> None
  | Some (types, fields) ->
      Some
        ( field.storage_key,
          doc (("bsonType", bson_type_element types) :: fields) )

let collection_validator_to_bson (entity : Ent_ocaml.entity) =
  let properties =
    entity.fields
    |> List.filter_map field_json_schema
    |> List.fold_left
         (fun acc (name, schema) ->
           Bson.add_element name (Bson.create_doc_element schema) acc)
         Bson.empty
  in
  let required =
    entity.fields
    |> List.filter (fun (field : Ent_ocaml.field) -> field.required)
    |> List.map (fun (field : Ent_ocaml.field) -> Bson.create_string field.storage_key)
  in
  let schema =
    doc
      [
        ("bsonType", Bson.create_string "object");
        ("properties", Bson.create_doc_element properties);
      ]
  in
  let schema =
    match required with
    | [] -> schema
    | fields -> Bson.add_element "required" (Bson.create_list fields) schema
  in
  doc [ ("$jsonSchema", Bson.create_doc_element schema) ]

let ensure_collection_validator ctx (entity : Ent_ocaml.entity) =
  let validator = collection_validator_to_bson entity in
  let command =
    [
      ("collMod", Bson.create_string entity.collection);
      ("validator", Bson.create_doc_element validator);
      ("validationLevel", Bson.create_string "moderate");
      ("validationAction", Bson.create_string "error");
    ]
  in
  match Mongo_eio.direct_run_command ctx.client ctx.config.database command with
  | Ok _ -> Ok ()
  | Error error when Mongo_error.code error = Some 26 ->
      let create_command =
        [
          ("create", Bson.create_string entity.collection);
          ("validator", Bson.create_doc_element validator);
          ("validationLevel", Bson.create_string "moderate");
          ("validationAction", Bson.create_string "error");
        ]
      in
      (match Mongo_eio.direct_run_command ctx.client ctx.config.database create_command with
      | Ok _ -> Ok ()
      | Error error -> Error (backend_error "create_collection_validator" entity error))
  | Error error -> Error (backend_error "coll_mod_validator" entity error)

let ensure_collection_validators ctx entities =
  let rec loop = function
    | [] -> Ok ()
    | entity :: rest -> (
        match ensure_collection_validator ctx entity with
        | Ok () -> loop rest
        | Error _ as error -> error)
  in
  loop entities

let collection_validator_check_ok check =
  match check.validator_status with
  | Validator_present -> true
  | Validator_missing | Validator_mismatched _ -> false

let collection_validator_check_to_string check =
  match check.validator_status with
  | Validator_present ->
      Printf.sprintf "%s validator present" check.validator_collection
  | Validator_missing ->
      Printf.sprintf "%s validator missing" check.validator_collection
  | Validator_mismatched reasons ->
      Printf.sprintf "%s validator mismatched: %s" check.validator_collection
        (String.concat "; " reasons)

let collection_actual ctx (entity : Ent_ocaml.entity) =
  let filter = doc [ ("name", Bson.create_string entity.collection) ] in
  match
    Mongo_eio.direct_run_command ctx.client ctx.config.database
      [
        ("listCollections", Bson.create_int32 1l);
        ("filter", Bson.create_doc_element filter);
        ("nameOnly", Bson.create_boolean false);
      ]
  with
  | Error error -> Error (backend_error "list_collections" entity error)
  | Ok response -> (
      match Mongo_command.cursor_batch response.Mongo_command.body with
      | [] -> Ok None
      | collection :: _ -> Ok (Some collection))

let collection_doc name bson =
  try Some (Bson.get_doc_element (Bson.get_element name bson)) with
  | Not_found | Bson.Wrong_bson_type -> None

let collection_string ~default name bson =
  try Bson.get_string (Bson.get_element name bson) with
  | Not_found | Bson.Wrong_bson_type -> default

let check_collection_validator ctx (entity : Ent_ocaml.entity) =
  match collection_actual ctx entity with
  | Error _ as error -> error
  | Ok None ->
      Ok
        {
          validator_entity = entity.name;
          validator_collection = entity.collection;
          validator_status = Validator_missing;
        }
  | Ok (Some collection) ->
      let expected = collection_validator_to_bson entity in
      let options = collection_doc "options" collection |> Option.value ~default:Bson.empty in
      let actual = collection_doc "validator" options in
      let mismatches = ref [] in
      let check name condition =
        if not condition then mismatches := name :: !mismatches
      in
      check "validator"
        (match actual with
        | Some actual -> doc_equal expected actual
        | None -> false);
      check "validationLevel"
        (collection_string ~default:"" "validationLevel" options = "moderate");
      check "validationAction"
        (collection_string ~default:"" "validationAction" options = "error");
      Ok
        {
          validator_entity = entity.name;
          validator_collection = entity.collection;
          validator_status =
            (match List.rev !mismatches with
            | [] -> Validator_present
            | mismatches -> Validator_mismatched mismatches);
        }

let check_collection_validators ctx entities =
  let rec loop acc = function
    | [] -> Ok (List.rev acc)
    | entity :: rest -> (
        match check_collection_validator ctx entity with
        | Error _ as error -> error
        | Ok check -> loop (check :: acc) rest)
  in
  loop [] entities

let verify_collection_validators ctx entities =
  match check_collection_validators ctx entities with
  | Error _ as error -> error
  | Ok checks -> (
      match List.filter (fun check -> not (collection_validator_check_ok check)) checks with
      | [] -> Ok ()
      | failed ->
          Error
            (`Bad_schema
              ("Mongo collection validator drift: "
              ^ String.concat ", "
                  (List.map collection_validator_check_to_string failed))))

let document_to_bson ?entity fields =
  let rec loop doc = function
    | [] -> Ok doc
    | (name, value) :: rest -> (
        match (predicate_field ?entity name, value_to_bson value) with
        | Ok name, Ok bson -> loop (Bson.add_element name bson doc) rest
        | Error _ as error, _ | _, (Error _ as error) -> error)
  in
  loop Bson.empty fields

let update_to_bson (mutation : Ent_ocaml.mutation) =
  let open Ent_ocaml in
  let add_if_any name doc update =
    if doc = Bson.empty then update
    else Bson.add_element name (Bson.create_doc_element doc) update
  in
  match
    ( document_to_bson ~entity:mutation.entity mutation.set,
      document_to_bson ~entity:mutation.entity mutation.add,
      document_to_bson ~entity:mutation.entity mutation.on_insert )
  with
  | Error _ as error, _, _
  | _, (Error _ as error), _
  | _, _, (Error _ as error) ->
      error
  | Ok set_doc, Ok inc_doc, Ok on_insert_doc ->
      let unset_doc =
        mutation.clear
        |> List.map (fun field -> (field, Bson.create_string ""))
        |> doc
      in
      let update =
        Bson.empty
        |> add_if_any "$set" set_doc
        |> add_if_any "$unset" unset_doc
        |> add_if_any "$inc" inc_doc
        |> add_if_any "$setOnInsert" on_insert_doc
      in
      if update = Bson.empty then bad_query "mutation has no update operations"
      else Ok update

let selector (mutation : Ent_ocaml.mutation) =
  match mutation.Ent_ocaml.predicates with
  | [] -> Ok Bson.empty
  | [ predicate ] -> predicate_to_bson ~entity:mutation.entity predicate
  | predicates -> predicate_to_bson ~entity:mutation.entity (And predicates)

let edge_order_lookup_stages (query : Ent_ocaml.query) index
    (order : Ent_ocaml.order) =
  let ({ Ent_ocaml.target = order_target; _ } : Ent_ocaml.order) = order in
  match order_target with
  | Ent_ocaml.Field_order _ -> Ok []
  | Edge_field_order { edge; target; field } -> (
      match find_edge query.Ent_ocaml.entity edge with
      | None -> Error (`Bad_query ("edge not found: " ^ edge))
      | Some edge_desc when edge_desc.target <> target.name ->
          Error
            (`Bad_query
              (Printf.sprintf "edge %s targets %s, not %s" edge_desc.name
                 edge_desc.target target.name))
      | Some { direction = From _; _ } ->
          Error (`Bad_query "edge-field ordering currently supports to-edges")
      | Some { cardinality = Many; _ } ->
          Error (`Bad_query "edge-field ordering currently supports to-one edges")
      | Some edge_desc -> (
          match
            ( edge_desc.storage_key,
              field_storage_key target "id",
              field_storage_key target field )
          with
          | Some local_key, Ok foreign_key, Ok target_field_key ->
              let temp = edge_order_temp index in
              let lookup =
                doc
                  [
                    ("from", Bson.create_string target.collection);
                    ("localField", Bson.create_string local_key);
                    ("foreignField", Bson.create_string foreign_key);
                    ("as", Bson.create_string temp);
                  ]
              in
              let unwind =
                doc
                  [
                    ("path", Bson.create_string ("$" ^ temp));
                    ("preserveNullAndEmptyArrays", Bson.create_boolean true);
                  ]
              in
              let alias_stage =
                match order.value_alias with
                | None -> []
                | Some alias ->
                    [
                      doc
                        [
                          ( "$addFields",
                            Bson.create_doc_element
                              (doc
                                 [
                                   ( alias,
                                     Bson.create_string
                                       ("$" ^ temp ^ "." ^ target_field_key) );
                                 ]) );
                        ];
                    ]
              in
              Ok
                ([
                   doc [ ("$lookup", Bson.create_doc_element lookup) ];
                   doc [ ("$unwind", Bson.create_doc_element unwind) ];
                 ]
                @ alias_stage)
          | None, _, _ ->
              Error
                (`Bad_query
                  ("edge " ^ edge_desc.name
                 ^ " does not have a stored foreign-key field"))
          | _, (Error _ as error), _ | _, _, (Error _ as error) -> error))
  | Edge_count_order { edge; target } -> (
      match find_edge query.Ent_ocaml.entity edge with
      | None -> Error (`Bad_query ("edge not found: " ^ edge))
      | Some edge_desc when edge_desc.target <> target.name ->
          Error
            (`Bad_query
              (Printf.sprintf "edge %s targets %s, not %s" edge_desc.name
                 edge_desc.target target.name))
      | Some { direction = From _; _ } ->
          Error (`Bad_query "edge-count ordering currently supports to-edges")
      | Some { cardinality = One; _ } ->
          Error
            (`Bad_query "edge-count ordering currently supports to-many edges")
      | Some edge_desc -> (
          match
            ( field_storage_key query.Ent_ocaml.entity "id",
              edge_desc.storage_key,
              edge_desc.join )
          with
          | Ok local_key, Some foreign_key, None ->
              let temp = edge_order_temp index in
              let count_key = temp ^ "_count" in
              let count_expr =
                Bson.create_doc_element
                  (doc [ ("$size", Bson.create_string ("$" ^ temp)) ])
              in
              let lookup =
                doc
                  [
                    ("from", Bson.create_string target.collection);
                    ("localField", Bson.create_string local_key);
                    ("foreignField", Bson.create_string foreign_key);
                    ("as", Bson.create_string temp);
                  ]
              in
              let fields =
                match order.value_alias with
                | None -> [ (count_key, count_expr) ]
                | Some alias -> [ (count_key, count_expr); (alias, count_expr) ]
              in
              Ok
                [
                  doc [ ("$lookup", Bson.create_doc_element lookup) ];
                  doc
                    [
                      ( "$addFields",
                        Bson.create_doc_element (doc fields) );
                    ];
                ]
          | Ok local_key, None, Some join ->
              let temp = edge_order_temp index in
              let count_key = temp ^ "_count" in
              let count_expr =
                Bson.create_doc_element
                  (doc [ ("$size", Bson.create_string ("$" ^ temp)) ])
              in
              let lookup =
                doc
                  [
                    ("from", Bson.create_string join.collection);
                    ("localField", Bson.create_string local_key);
                    ("foreignField", Bson.create_string join.source_key);
                    ("as", Bson.create_string temp);
                  ]
              in
              let fields =
                match order.value_alias with
                | None -> [ (count_key, count_expr) ]
                | Some alias -> [ (count_key, count_expr); (alias, count_expr) ]
              in
              Ok
                [
                  doc [ ("$lookup", Bson.create_doc_element lookup) ];
                  doc
                    [
                      ( "$addFields",
                        Bson.create_doc_element (doc fields) );
                    ];
                ]
          | Error _ as error, _, _ -> error
          | _, Some _, Some _ ->
              Error
                (`Bad_query
                  ("edge " ^ edge_desc.name
                 ^ " must use either storage_key or join metadata, not both"))
          | _, None, None ->
              Error
                (`Bad_query
                  ("edge " ^ edge_desc.name
                 ^ " does not have a stored foreign-key field"))))
  | Backend_order { backend; term; _ } ->
      if backend <> "mongo" then
        Error (`Bad_query ("unsupported backend order: " ^ backend))
      else
        let key = backend_order_temp index in
        match value_to_bson term with
        | Error _ as error -> error
        | Ok bson ->
            let fields =
              match order.value_alias with
              | None -> [ (key, bson) ]
              | Some alias -> [ (key, bson); (alias, bson) ]
            in
            Ok
              [
                doc
                  [
                    ( "$addFields",
                      Bson.create_doc_element (doc fields) );
                  ];
              ]

let edge_order_lookup_pipeline query =
  let rec loop index acc = function
    | [] -> Ok (List.rev acc)
    | order :: rest -> (
        match edge_order_lookup_stages query index order with
        | Ok stages -> loop (index + 1) (List.rev_append stages acc) rest
        | Error _ as error -> error)
  in
  loop 0 [] query.Ent_ocaml.orders

let find_aggregate_pipeline (query : Ent_ocaml.query) =
  let split_predicates =
    partition_edge_target_predicates query.Ent_ocaml.predicates
  in
  match
    ( split_predicates,
      edge_order_lookup_pipeline query,
      sort_to_bson_result query,
      projection_to_bson query )
  with
  | Error _ as error, _, _, _
  | _, (Error _ as error), _, _
  | _, _, (Error _ as error), _
  | _, _, _, (Error _ as error) ->
      error
  | Ok (edge_predicates, normal_predicates), Ok lookup_stages, Ok sort, Ok projection -> (
      match
        partition_join_edge_predicates query normal_predicates
      with
      | Error _ as error -> error
      | Ok (join_predicates, normal_predicates) -> (
          match
            ( filter_to_bson { query with predicates = normal_predicates },
              edge_target_predicate_lookup_pipeline query edge_predicates,
              join_edge_predicate_lookup_pipeline query join_predicates )
          with
          | Error _ as error, _, _
          | _, (Error _ as error), _
          | _, _, (Error _ as error) ->
              error
          | Ok filter, Ok edge_predicate_stages, Ok join_predicate_stages ->
      let match_stage =
        if filter = Bson.empty then []
        else [ doc [ ("$match", Bson.create_doc_element filter) ] ]
      in
      let sort_stage =
        match sort with
        | None -> []
        | Some sort -> [ doc [ ("$sort", Bson.create_doc_element sort) ] ]
      in
      let skip_stage =
        match query.offset with
        | None -> []
        | Some offset -> [ doc [ ("$skip", Bson.create_int64 (Int64.of_int offset)) ] ]
      in
      let limit_stage =
        match query.limit with
        | None -> []
        | Some limit -> [ doc [ ("$limit", Bson.create_int64 (Int64.of_int limit)) ] ]
      in
      let project_stage =
        match projection with
        | None -> []
        | Some projection ->
            [ doc [ ("$project", Bson.create_doc_element projection) ] ]
      in
      Ok
        (match_stage @ edge_predicate_stages @ join_predicate_stages
       @ lookup_stages @ sort_stage
       @ skip_stage @ limit_stage @ project_stage)))

let find_with_aggregate ctx query =
  match find_aggregate_pipeline query with
  | Error _ as error -> error
  | Ok pipeline -> (
      let session = transaction_session ctx in
      match
        Mongo_eio.direct_run_command ?session ctx.client ctx.config.database
          [
            ("aggregate", Bson.create_string query.Ent_ocaml.entity.collection);
            ("pipeline", Bson.create_doc_element_list pipeline);
            ("cursor", Bson.create_doc_element Bson.empty);
          ]
      with
      | Error error -> Error (backend_error "aggregate find" query.entity error)
      | Ok response -> Ok (Mongo_command.cursor_batch response.Mongo_command.body))

let find ctx (query : Ent_ocaml.query) =
  if
    needs_aggregate_order query || has_edge_target_predicate query
    || has_join_edge_predicate query
  then
    find_with_aggregate ctx query
  else
    match find_options query with
    | Error _ as error -> error
    | Ok options -> (
        let session = transaction_session ctx in
        match
          Mongo_eio.direct_find ?session ctx.client ~db:ctx.config.database
            ~collection:query.Ent_ocaml.entity.collection options
        with
        | Ok docs -> Ok docs
        | Error error -> Error (backend_error "find" query.entity error))

let decode_document ~decode doc =
  decode doc |> Result.map_error (fun message -> `Decode message)

let decode_documents ~decode docs =
  let rec loop acc = function
    | [] -> Ok (List.rev acc)
    | doc :: rest -> (
        match decode_document ~decode doc with
        | Ok value -> loop (value :: acc) rest
        | Error _ as error -> error)
  in
  loop [] docs

let find_one ctx query =
  let query = { query with Ent_ocaml.limit = Some 1 } in
  match find ctx query with
  | Ok [] -> Ok None
  | Ok (doc :: _) -> Ok (Some doc)
  | Error _ as error -> error

let find_as ctx query ~decode =
  match find ctx query with
  | Ok docs -> decode_documents ~decode docs
  | Error _ as error -> error

let find_one_as ctx query ~decode =
  match find_one ctx query with
  | Ok None -> Ok None
  | Ok (Some doc) -> decode_document ~decode doc |> Result.map Option.some
  | Error _ as error -> error

let count ctx (query : Ent_ocaml.query) =
  match filter_to_bson query with
  | Error _ as error -> error
  | Ok filter -> (
      let session = transaction_session ctx in
      match
        Mongo_eio.direct_count_documents ?session ctx.client ~db:ctx.config.database
          ~collection:query.Ent_ocaml.entity.collection ~query:filter ()
      with
      | Ok count -> Ok count
      | Error error -> Error (backend_error "count" query.entity error))

let value_of_bson_element element =
  let decoders =
    [
      (fun () ->
        try Some (Ent_ocaml.V_string (Bson.get_string element)) with _ -> None);
      (fun () ->
        try Some (Ent_ocaml.V_int32 (Bson.get_int32 element)) with _ -> None);
      (fun () ->
        try Some (Ent_ocaml.V_int64 (Bson.get_int64 element)) with _ -> None);
      (fun () ->
        try Some (Ent_ocaml.V_float (Bson.get_double element)) with _ -> None);
      (fun () ->
        try Some (Ent_ocaml.V_bool (Bson.get_boolean element)) with _ -> None);
    ]
  in
  let is_null =
    try
      ignore (Bson.get_null element);
      true
    with
    | _ -> false
  in
  let rec loop = function
    | [] ->
        if is_null then Ok Ent_ocaml.V_null
        else Error (`Decode "unsupported aggregate BSON value")
    | decode :: rest -> (
        match decode () with
        | Some value -> Ok value
        | None -> loop rest)
  in
  loop decoders

let bson_element_at_path doc key =
  let direct () = Bson.get_element key doc in
  let nested () =
    match String.split_on_char '.' key with
    | [] -> raise Not_found
    | root :: path ->
        let rec loop current = function
          | [] -> raise Not_found
          | [ segment ] -> Bson.get_element segment current
          | segment :: rest ->
              loop (Bson.get_doc_element (Bson.get_element segment current)) rest
        in
        loop doc (root :: path)
  in
  try direct () with
  | _ -> nested ()

let selected_fields (query : Ent_ocaml.query) =
  let fields =
    List.map
      (fun field -> (field, fun () -> field_storage_key query.entity field))
      query.select
  in
  let order_values =
    query.orders
    |> List.filter_map (fun ({ target = order_target; value_alias; _ } : Ent_ocaml.order) ->
           Option.map
             (fun alias ->
               ( alias,
                 fun () ->
                   match order_target with
                   | Ent_ocaml.Field_order field ->
                       order_storage_key query.entity field
                   | Edge_field_order _ | Edge_count_order _ | Backend_order _ ->
                       Ok alias ))
             value_alias)
  in
  match fields @ order_values with
  | [] -> Error (`Bad_query "values expects selected fields or order values")
  | fields -> Ok fields

let selected_row (query : Ent_ocaml.query) doc =
  let rec loop acc = function
    | [] -> Ok (Ent_ocaml.V_doc (List.rev acc))
    | (label, storage_key) :: rest -> (
        match storage_key () with
        | Error _ as error -> error
        | Ok key -> (
            try
              match value_of_bson_element (bson_element_at_path doc key) with
              | Ok value -> loop ((label, value) :: acc) rest
              | Error _ as error -> error
            with
            | _ -> Error (`Decode ("selected field missing: " ^ label))))
  in
  match selected_fields query with
  | Error _ as error -> error
  | Ok fields -> loop [] fields

let selected_rows (query : Ent_ocaml.query) docs =
  let rec loop acc = function
    | [] -> Ok (List.rev acc)
    | doc :: rest -> (
        match selected_row query doc with
        | Ok row -> loop (row :: acc) rest
        | Error _ as error -> error)
  in
  loop [] docs

let selected_value (query : Ent_ocaml.query) doc =
  match selected_fields query with
  | Ok [ (field, _) ] -> (
      match selected_row query doc with
      | Ok (Ent_ocaml.V_doc fields) -> Ok (List.assoc field fields)
      | Ok _ -> Error (`Decode "selected row is not a document")
      | Error _ as error -> error)
  | Ok [] | Ok (_ :: _ :: _) ->
      Error (`Bad_query "value expects one selected field or order value")
  | Error _ as error -> error

let values ctx query =
  match find ctx query with
  | Error _ as error -> error
  | Ok docs -> selected_rows query docs

let value ctx query =
  match find_one ctx query with
  | Error _ as error -> error
  | Ok None -> Ok None
  | Ok (Some doc) -> selected_value query doc |> Result.map Option.some

type stored_edge =
  | Stored_to_one of { source_fk_key : string }
  | Stored_to_many of { source_id_key : string; target_fk_key : string }
  | Join_to_many of { source_id_key : string; join : Ent_ocaml.edge_join }

let stored_edge (edge_query : Ent_ocaml.edge_query) =
  let open Ent_ocaml in
  let source_entity = edge_query.source.entity in
  match find_edge source_entity edge_query.edge with
  | None -> Error (`Bad_query ("edge not found: " ^ edge_query.edge))
  | Some edge when edge.target <> edge_query.target.name ->
      Error
        (`Bad_query
          (Printf.sprintf "edge %s targets %s, not %s" edge.name edge.target
             edge_query.target.name))
  | Some { direction = From _; _ } ->
      Error (`Bad_query "edge traversal currently supports to-edges")
  | Some edge -> (
      match (edge.cardinality, edge.storage_key, edge.join) with
      | One, Some storage_key, None ->
          Ok (Stored_to_one { source_fk_key = storage_key })
      | Many, Some storage_key, None -> (
          match field_storage_key source_entity "id" with
          | Ok source_id_key ->
              Ok
                (Stored_to_many
                   { source_id_key; target_fk_key = storage_key })
          | Error _ as error -> error)
      | Many, None, Some join -> (
          match field_storage_key source_entity "id" with
          | Ok source_id_key -> Ok (Join_to_many { source_id_key; join })
          | Error _ as error -> error)
      | One, _, Some _ ->
          Error (`Bad_query ("edge " ^ edge.name ^ " has join metadata but is not to-many"))
      | Many, Some _, Some _ ->
          Error
            (`Bad_query
              ("edge " ^ edge.name
             ^ " must use either storage_key or join metadata, not both"))
      | _, None, None ->
          Error
            (`Bad_query
              ("edge " ^ edge.name ^ " does not have a stored foreign-key field")))

let document_value storage_key doc =
  match Bson.get_element storage_key doc with
  | exception Not_found -> Ok None
  | element -> (
      match value_of_bson_element element with
      | Error _ as error -> error
      | Ok Ent_ocaml.V_null -> Ok None
      | Ok value -> Ok (Some value))

let collect_document_values storage_key docs =
  let rec loop acc = function
    | [] -> Ok (List.rev acc)
    | doc :: rest -> (
        match document_value storage_key doc with
        | Error _ as error -> error
        | Ok None -> loop acc rest
        | Ok (Some value) ->
            let acc = if List.mem value acc then acc else value :: acc in
            loop acc rest)
  in
  loop [] docs

let foreign_key_value = document_value
let collect_foreign_keys = collect_document_values

let find_collection_docs ctx ~collection filter =
  let options = Mongo_crud.default_find collection filter in
  let session = transaction_session ctx in
  match
    Mongo_eio.direct_find ?session ctx.client ~db:ctx.config.database ~collection
      options
  with
  | Ok docs -> Ok docs
  | Error error ->
      Error
        (`Backend
          (Printf.sprintf "find_join %s: %s" collection
             (Mongo_error.to_string error)))

let find_join_docs ctx (join : Ent_ocaml.edge_join) source_ids =
  match source_ids with
  | [] -> Ok []
  | source_ids -> (
      match op_doc join.source_key "$in" (Ent_ocaml.V_list source_ids) with
      | Error _ as error -> error
      | Ok filter -> find_collection_docs ctx ~collection:join.collection filter)

let find_traversal_targets ctx edge_query ids =
  match ids with
  | [] -> Ok []
  | ids ->
      let target_query =
        edge_query.Ent_ocaml.target_query
        |> Ent_ocaml.Query.where (Ent_ocaml.In ("id", ids))
      in
      find ctx target_query

let find_to_many_targets ctx edge_query target_fk_key source_ids =
  match source_ids with
  | [] -> Ok []
  | source_ids ->
      let target_query =
        edge_query.Ent_ocaml.target_query
        |> Ent_ocaml.Query.where (Ent_ocaml.In (target_fk_key, source_ids))
      in
      find ctx target_query

let join_source_target_ids join join_docs =
  let rec loop acc = function
    | [] -> Ok (List.rev acc)
    | doc :: rest -> (
        match
          ( document_value join.Ent_ocaml.source_key doc,
            document_value join.target_key doc )
        with
        | Error _ as error, _ | _, (Error _ as error) -> error
        | Ok (Some source_id), Ok (Some target_id) ->
            loop ((source_id, target_id) :: acc) rest
        | Ok None, _ | _, Ok None -> loop acc rest)
  in
  loop [] join_docs

let collect_join_target_ids join_docs join =
  collect_document_values join.Ent_ocaml.target_key join_docs

let traverse_as ctx (edge_query : Ent_ocaml.edge_query) ~decode =
  match stored_edge edge_query with
  | Error _ as error -> error
  | Ok (Stored_to_one { source_fk_key }) -> (
      let source_query = { edge_query.source with Ent_ocaml.select = [] } in
      match find ctx source_query with
      | Error _ as error -> error
      | Ok source_docs -> (
          match collect_foreign_keys source_fk_key source_docs with
          | Error _ as error -> error
          | Ok ids -> (
              match find_traversal_targets ctx edge_query ids with
              | Error _ as error -> error
              | Ok target_docs -> decode_documents ~decode target_docs)))
  | Ok (Stored_to_many { source_id_key; target_fk_key }) -> (
      let source_query = { edge_query.source with Ent_ocaml.select = [] } in
      match find ctx source_query with
      | Error _ as error -> error
      | Ok source_docs -> (
          match collect_document_values source_id_key source_docs with
          | Error _ as error -> error
          | Ok source_ids -> (
              match find_to_many_targets ctx edge_query target_fk_key source_ids with
              | Error _ as error -> error
              | Ok target_docs -> decode_documents ~decode target_docs)))
  | Ok (Join_to_many { source_id_key; join }) -> (
      let source_query = { edge_query.source with Ent_ocaml.select = [] } in
      match find ctx source_query with
      | Error _ as error -> error
      | Ok source_docs -> (
          match collect_document_values source_id_key source_docs with
          | Error _ as error -> error
          | Ok source_ids -> (
              match find_join_docs ctx join source_ids with
              | Error _ as error -> error
              | Ok join_docs -> (
                  match collect_join_target_ids join_docs join with
                  | Error _ as error -> error
                  | Ok target_ids -> (
                      match find_traversal_targets ctx edge_query target_ids with
                      | Error _ as error -> error
                      | Ok target_docs -> decode_documents ~decode target_docs)))))

let load_edge_as ctx (edge_query : Ent_ocaml.edge_query) ~decode_source
    ~decode_target =
  match stored_edge edge_query with
  | Error _ as error -> error
  | Ok (Stored_to_one { source_fk_key }) -> (
      let source_query = { edge_query.source with Ent_ocaml.select = [] } in
      match find ctx source_query with
      | Error _ as error -> error
      | Ok source_docs -> (
          match collect_foreign_keys source_fk_key source_docs with
          | Error _ as error -> error
          | Ok ids -> (
              match
                ( find_traversal_targets ctx edge_query ids,
                  field_storage_key edge_query.target "id" )
              with
              | Error _ as error, _ | _, (Error _ as error) -> error
              | Ok target_docs, Ok target_id_key ->
                  let target_value doc =
                    match Bson.get_element target_id_key doc with
                    | exception Not_found ->
                        Error (`Decode "target id field missing")
                    | element -> value_of_bson_element element
                  in
                  let rec target_map acc = function
                    | [] -> Ok acc
                    | doc :: rest -> (
                        match target_value doc with
                        | Error _ as error -> error
                        | Ok id -> target_map ((id, doc) :: acc) rest)
                  in
                  let decode_target_option fk targets =
                    match List.assoc_opt fk targets with
                    | None -> Ok None
                    | Some doc ->
                        decode_document ~decode:decode_target doc
                        |> Result.map Option.some
                  in
                  let rec pair_rows targets acc = function
                    | [] -> Ok (List.rev acc)
                    | source_doc :: rest -> (
                        match
                          ( decode_document ~decode:decode_source source_doc,
                            foreign_key_value source_fk_key source_doc )
                        with
                        | Error _ as error, _ | _, (Error _ as error) -> error
                        | Ok source, Ok None ->
                            pair_rows targets ((source, None) :: acc) rest
                        | Ok source, Ok (Some fk) -> (
                            match decode_target_option fk targets with
                            | Error _ as error -> error
                            | Ok target ->
                                pair_rows targets ((source, target) :: acc) rest))
                  in
                  match target_map [] target_docs with
                  | Error _ as error -> error
                  | Ok targets -> pair_rows targets [] source_docs)))
  | Ok (Stored_to_many { source_id_key; target_fk_key }) -> (
      let source_query = { edge_query.source with Ent_ocaml.select = [] } in
      match find ctx source_query with
      | Error _ as error -> error
      | Ok source_docs -> (
          match collect_document_values source_id_key source_docs with
          | Error _ as error -> error
          | Ok source_ids -> (
              match find_to_many_targets ctx edge_query target_fk_key source_ids with
              | Error _ as error -> error
              | Ok target_docs ->
                  let target_matches source_id =
                    let rec loop acc = function
                      | [] -> Ok (List.rev acc)
                      | target_doc :: rest -> (
                          match document_value target_fk_key target_doc with
                          | Error _ as error -> error
                          | Ok (Some fk) when fk = source_id ->
                              loop (target_doc :: acc) rest
                          | Ok (Some _) | Ok None -> loop acc rest)
                    in
                    loop [] target_docs
                  in
                  let append_targets source acc = function
                    | [] -> Ok ((source, None) :: acc)
                    | targets ->
                        let rec loop acc = function
                          | [] -> Ok acc
                          | target_doc :: rest -> (
                              match
                                decode_document ~decode:decode_target target_doc
                              with
                              | Error _ as error -> error
                              | Ok target ->
                                  loop ((source, Some target) :: acc) rest)
                        in
                        loop acc targets
                  in
                  let rec pair_rows acc = function
                    | [] -> Ok (List.rev acc)
                    | source_doc :: rest -> (
                        match
                          ( decode_document ~decode:decode_source source_doc,
                            document_value source_id_key source_doc )
                        with
                        | Error _ as error, _ | _, (Error _ as error) -> error
                        | Ok source, Ok None ->
                            pair_rows ((source, None) :: acc) rest
                        | Ok source, Ok (Some source_id) -> (
                            match target_matches source_id with
                            | Error _ as error -> error
                            | Ok targets -> (
                                match append_targets source acc targets with
                                | Error _ as error -> error
                                | Ok acc -> pair_rows acc rest)))
                  in
                  pair_rows [] source_docs)))
  | Ok (Join_to_many { source_id_key; join }) -> (
      let source_query = { edge_query.source with Ent_ocaml.select = [] } in
      match find ctx source_query with
      | Error _ as error -> error
      | Ok source_docs -> (
          match collect_document_values source_id_key source_docs with
          | Error _ as error -> error
          | Ok source_ids -> (
              match find_join_docs ctx join source_ids with
              | Error _ as error -> error
              | Ok join_docs -> (
                  match
                    ( join_source_target_ids join join_docs,
                      collect_join_target_ids join_docs join,
                      field_storage_key edge_query.target "id" )
                  with
                  | Error _ as error, _, _
                  | _, (Error _ as error), _
                  | _, _, (Error _ as error) ->
                      error
                  | Ok pairs, Ok target_ids, Ok target_id_key -> (
                      match find_traversal_targets ctx edge_query target_ids with
                      | Error _ as error -> error
                      | Ok target_docs ->
                          let target_id doc =
                            match Bson.get_element target_id_key doc with
                            | exception Not_found ->
                                Error (`Decode "target id field missing")
                            | element -> value_of_bson_element element
                          in
                          let target_matches source_id =
                            let rec loop acc = function
                              | [] -> Ok (List.rev acc)
                              | target_doc :: rest -> (
                                  match target_id target_doc with
                                  | Error _ as error -> error
                                  | Ok target_id
                                    when List.exists
                                           (fun (source, target) ->
                                             source = source_id
                                             && target = target_id)
                                           pairs ->
                                      loop (target_doc :: acc) rest
                                  | Ok _ -> loop acc rest)
                            in
                            loop [] target_docs
                          in
                          let append_targets source acc = function
                            | [] -> Ok ((source, None) :: acc)
                            | targets ->
                                let rec loop acc = function
                                  | [] -> Ok acc
                                  | target_doc :: rest -> (
                                      match
                                        decode_document ~decode:decode_target
                                          target_doc
                                      with
                                      | Error _ as error -> error
                                      | Ok target ->
                                          loop ((source, Some target) :: acc)
                                            rest)
                                in
                                loop acc targets
                          in
                          let rec pair_rows acc = function
                            | [] -> Ok (List.rev acc)
                            | source_doc :: rest -> (
                                match
                                  ( decode_document ~decode:decode_source
                                      source_doc,
                                    document_value source_id_key source_doc )
                                with
                                | Error _ as error, _ | _, (Error _ as error)
                                  ->
                                    error
                                | Ok source, Ok None ->
                                    pair_rows ((source, None) :: acc) rest
                                | Ok source, Ok (Some source_id) -> (
                                    match target_matches source_id with
                                    | Error _ as error -> error
                                    | Ok targets -> (
                                        match append_targets source acc targets with
                                        | Error _ as error -> error
                                        | Ok acc -> pair_rows acc rest)))
                          in
                          pair_rows [] source_docs)))))

let run_aggregate ctx (query : Ent_ocaml.query) pipeline =
  let session = transaction_session ctx in
  match
    Mongo_eio.direct_run_command ?session ctx.client ctx.config.database
      [
        ("aggregate", Bson.create_string query.Ent_ocaml.entity.collection);
        ("pipeline", Bson.create_doc_element_list pipeline);
        ("cursor", Bson.create_doc_element Bson.empty);
      ]
  with
  | Error error -> Error (backend_error "aggregate" query.Ent_ocaml.entity error)
  | Ok response -> Ok (Mongo_command.cursor_batch response.Mongo_command.body)

let aggregate ctx (aggregate : Ent_ocaml.aggregate) =
  let query = aggregate.Ent_ocaml.query in
  match aggregate_pipeline_to_bson aggregate with
  | Error _ as error -> error
  | Ok pipeline -> (
      match run_aggregate ctx query pipeline with
      | Error _ as error -> error
      | Ok [] -> Ok None
      | Ok (doc :: _) -> (
          match Bson.get_element "value" doc with
          | value -> value_of_bson_element value |> Result.map Option.some
          | exception Not_found -> Error (`Decode "aggregate value missing")))

let aggregate_scan ctx (scan : Ent_ocaml.aggregate_scan) =
  let query = scan.Ent_ocaml.query in
  match aggregate_scan_pipeline_to_bson scan with
  | Error _ as error -> error
  | Ok pipeline -> (
      match run_aggregate ctx query pipeline with
      | Error _ as error -> error
      | Ok [] -> Ok (List.map (fun (name, _) -> (name, None)) scan.ops)
      | Ok (doc :: _) ->
          let decode (name, _) =
            match Bson.get_element name doc with
            | exception Not_found -> Ok (name, None)
            | element ->
                value_of_bson_element element
                |> Result.map (fun value -> (name, Some value))
          in
          let rec loop acc = function
            | [] -> Ok (List.rev acc)
            | op :: rest -> (
                match decode op with
                | Ok value -> loop (value :: acc) rest
                | Error _ as error -> error)
          in
          loop [] scan.ops)

let group ctx (group : Ent_ocaml.group_aggregate) =
  let query = group.Ent_ocaml.aggregate.query in
  let decode doc =
    match Bson.get_element "_id" doc with
    | exception Not_found -> Error (`Decode "aggregate group missing")
    | group_value -> (
        match value_of_bson_element group_value with
        | Error _ as error -> error
        | Ok group_value -> (
            match Bson.get_element "value" doc with
            | exception Not_found -> Ok { Ent_ocaml.group = group_value; value = None }
            | value -> (
                match value_of_bson_element value with
                | Error _ as error -> error
                | Ok value ->
                    Ok { Ent_ocaml.group = group_value; value = Some value })))
  in
  match group_pipeline_to_bson group with
  | Error _ as error -> error
  | Ok pipeline -> (
      match run_aggregate ctx query pipeline with
      | Error _ as error -> error
      | Ok docs ->
          let rec loop acc = function
            | [] -> Ok (List.rev acc)
            | doc :: rest -> (
                match decode doc with
                | Ok result -> loop (result :: acc) rest
                | Error _ as error -> error)
          in
          loop [] docs)

let insert ctx entity doc =
  let session = transaction_session ctx in
  match
    Mongo_eio.direct_insert_one ?session ctx.client ~db:ctx.config.database
      ~collection:entity.Ent_ocaml.collection doc
  with
  | Ok _ -> Ok doc
  | Error error when Mongo_error.is_duplicate_key error ->
      Error (`Constraint "duplicate key")
  | Error error -> Error (backend_error "insert" entity error)

let insert_many ?(ordered = true) ctx entity docs =
  match docs with
  | [] -> Ok []
  | _ -> (
      let options = Mongo_crud.{ default_insert with ordered } in
      let session = transaction_session ctx in
      match
        Mongo_eio.direct_insert_many ?session ctx.client ~db:ctx.config.database
          ~collection:entity.Ent_ocaml.collection ~options docs
      with
      | Ok _ -> Ok docs
      | Error error when Mongo_error.is_duplicate_key error ->
          Error (`Constraint "duplicate key")
      | Error error -> Error (backend_error "insert_many" entity error))

let insert_values ctx (mutation : Ent_ocaml.mutation) =
  match mutation.op with
  | Ent_ocaml.Create -> (
      match Ent_ocaml.validate_mutation mutation with
      | Error _ as error -> error
      | Ok () -> (
          match document_to_bson ~entity:mutation.entity mutation.set with
          | Error _ as error -> error
          | Ok doc -> insert ctx mutation.entity doc))
  | Update_one | Update | Delete_one | Delete | Upsert_one ->
      Error (`Bad_query "insert_values expects Create mutation op")

let insert_many_values ?ordered ctx mutations =
  let rec loop entity acc = function
    | [] -> (
        match entity with
        | None -> Ok []
        | Some entity -> insert_many ?ordered ctx entity (List.rev acc))
    | (mutation : Ent_ocaml.mutation) :: rest -> (
        match mutation.op with
        | Ent_ocaml.Create -> (
            match Ent_ocaml.validate_mutation mutation with
            | Error _ as error -> error
            | Ok () ->
                let same_entity =
                  match entity with
                  | None -> true
                  | Some entity ->
                      entity.name = mutation.entity.name
                      && entity.collection = mutation.entity.collection
                in
                if not same_entity then
                  Error (`Bad_query "insert_many_values expects one entity")
                else
                  match document_to_bson ~entity:mutation.entity mutation.set with
                  | Error _ as error -> error
                  | Ok doc ->
                      loop (Some mutation.entity) (doc :: acc) rest)
        | Update_one | Update | Delete_one | Delete | Upsert_one ->
            Error (`Bad_query "insert_many_values expects Create mutation ops"))
  in
  loop None [] mutations

let update ctx (mutation : Ent_ocaml.mutation) =
  let entity = mutation.Ent_ocaml.entity in
  match mutation.op with
  | Create | Delete_one | Delete | Upsert_one ->
      Error (`Bad_query "update expects Update_one or Update mutation op")
  | Update_one | Update -> (
      match Ent_ocaml.validate_mutation mutation with
      | Error _ as error -> error
      | Ok () -> (
          match (selector mutation, update_to_bson mutation) with
          | Error _ as error, _ | _, (Error _ as error) -> error
          | Ok selector, Ok update_doc -> (
          let session = transaction_session ctx in
          let run =
            match mutation.op with
            | Ent_ocaml.Update_one ->
                Mongo_eio.direct_update_one ?session ctx.client ~db:ctx.config.database
                  ~collection:entity.collection ~upsert:false selector
                  update_doc
            | Update ->
                Mongo_eio.direct_update_many ?session ctx.client ~db:ctx.config.database
                  ~collection:entity.collection ~upsert:false selector
                  update_doc
            | Create | Delete_one | Delete | Upsert_one -> assert false
          in
          match run with
          | Ok result ->
              Ok
                (Option.value result.Mongo_crud.modified_count
                   ~default:result.matched_count)
          | Error error -> Error (backend_error "update" entity error))))

let update_one ctx (mutation : Ent_ocaml.mutation) =
  let entity = mutation.Ent_ocaml.entity in
  match mutation.op with
  | Create | Update | Delete_one | Delete | Upsert_one ->
      Error (`Bad_query "update_one expects Update_one mutation op")
  | Update_one -> (
      match Ent_ocaml.validate_mutation mutation with
      | Error _ as error -> error
      | Ok () -> (
          match (selector mutation, update_to_bson mutation) with
          | Error _ as error, _ | _, (Error _ as error) -> error
          | Ok selector, Ok update_doc -> (
          let session = transaction_session ctx in
          match
            Mongo_eio.direct_update_one ?session ctx.client ~db:ctx.config.database
              ~collection:entity.collection ~upsert:false selector update_doc
          with
          | Ok result ->
              if result.Mongo_crud.matched_count = 0 then Error `Not_found
              else Ok ()
          | Error error -> Error (backend_error "update_one" entity error))))

let upsert_one ctx (mutation : Ent_ocaml.mutation) =
  let entity = mutation.Ent_ocaml.entity in
  match mutation.op with
  | Create | Update_one | Update | Delete_one | Delete ->
      Error (`Bad_query "upsert_one expects Upsert_one mutation op")
  | Upsert_one -> (
      match Ent_ocaml.validate_mutation mutation with
      | Error _ as error -> error
      | Ok () -> (
          match (selector mutation, update_to_bson mutation) with
          | Error _ as error, _ | _, (Error _ as error) -> error
          | Ok selector, Ok update_doc -> (
              let session = transaction_session ctx in
              match
                Mongo_eio.direct_update_one ?session ctx.client ~db:ctx.config.database
                  ~collection:entity.collection ~upsert:true selector update_doc
              with
              | Ok _ -> Ok ()
              | Error error -> Error (backend_error "upsert_one" entity error))))

let delete ctx (mutation : Ent_ocaml.mutation) =
  let entity = mutation.Ent_ocaml.entity in
  match mutation.op with
  | Create | Update_one | Update | Upsert_one ->
      Error (`Bad_query "delete expects Delete_one or Delete mutation op")
  | Delete_one | Delete -> (
      match Ent_ocaml.validate_mutation mutation with
      | Error _ as error -> error
      | Ok () -> (
          match selector mutation with
          | Error _ as error -> error
          | Ok selector -> (
          let session = transaction_session ctx in
          let run =
            match mutation.op with
            | Ent_ocaml.Delete_one ->
                Mongo_eio.direct_delete_one ?session ctx.client ~db:ctx.config.database
                  ~collection:entity.collection selector
            | Delete ->
                Mongo_eio.direct_delete_many ?session ctx.client ~db:ctx.config.database
                  ~collection:entity.collection selector
            | Create | Update_one | Update | Upsert_one -> assert false
          in
          match run with
          | Ok result -> Ok result.Mongo_crud.deleted_count
          | Error error -> Error (backend_error "delete" entity error))))

let mongo_write_concern_w = function
  | Ent_ocaml.Write_majority -> `Majority
  | Write_nodes nodes -> `Nodes nodes
  | Write_tag tag -> `Tag tag

let mongo_write_concern (concern : Ent_ocaml.transaction_write_concern) =
  Mongo_command.
    {
      w = Option.map mongo_write_concern_w concern.write_w;
      j = concern.write_journal;
      wtimeout_ms = concern.write_wtimeout_ms;
    }

let transaction_command_fields name options =
  let fields =
    match (name, options) with
    | "commitTransaction", Some { Ent_ocaml.max_commit_time_ms = Some ms; _ } ->
        [
          (name, Bson.create_int32 1l);
          ("maxTimeMS", Bson.create_int64 (Int64.of_int ms));
        ]
    | _ -> [ (name, Bson.create_int32 1l) ]
  in
  match (name, options) with
  | "commitTransaction", Some { Ent_ocaml.write_concern = Some concern; _ } ->
      fields
      @ [
          ( "writeConcern",
            Bson.create_doc_element
              (Mongo_command.write_concern_doc (mongo_write_concern concern)) );
        ]
  | _ -> fields

let transaction_command_to_bson name options =
  doc (transaction_command_fields name options)

let transaction_command ctx tx name =
  let session =
    Mongo_session.transaction_context tx.session ~txn_number:tx.txn_number
  in
  let command = transaction_command_fields name tx.options in
  Mongo_eio.direct_run_command ~session ctx.client "admin"
    command

let transaction ?options ctx f =
  match ctx.transaction with
  | Some _ -> f ctx
  | None ->
      let tx =
        let session = Mongo_session.create () in
        {
          session;
          txn_number = Mongo_session.next_txn session;
          options;
          started = false;
        }
      in
      let tx_ctx = { ctx with transaction = Some tx } in
      match f tx_ctx with
      | Ok value ->
          if not tx.started then Ok value
          else (
            match transaction_command ctx tx "commitTransaction" with
            | Ok _ -> Ok value
            | Error error ->
                Error
                  (`Backend
                    ("commit_transaction: " ^ Mongo_error.to_string error)))
      | Error error ->
          if tx.started then ignore (transaction_command ctx tx "abortTransaction");
          Error error
