type config = {
  database : string;
}

type ctx = {
  client : Mongo_eio.direct_client;
  config : config;
}

type doc = Bson.t

let create ~client config = { client; config }

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
  match orders with
  | [] -> None
  | orders ->
      let fields =
        List.map
          (fun (order : Ent_ocaml.order) ->
            let field = order_field ?entity order.Ent_ocaml.field in
            (field, direction order.direction))
          orders
      in
      Some (doc fields)

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
    | { field; value_alias = Some _; _ } :: rest -> (
        match order_storage_key query.entity field with
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

let find ctx (query : Ent_ocaml.query) =
  match find_options query with
  | Error _ as error -> error
  | Ok options -> (
      match
        Mongo_eio.direct_find ctx.client ~db:ctx.config.database
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
      match
        Mongo_eio.direct_count_documents ctx.client ~db:ctx.config.database
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
    |> List.filter_map (fun ({ field; value_alias; _ } : Ent_ocaml.order) ->
           Option.map
             (fun alias -> (alias, fun () -> order_storage_key query.entity field))
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

let stored_to_one_edge (edge_query : Ent_ocaml.edge_query) =
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
      Error (`Bad_query "stored-FK traversal currently supports to-edges")
  | Some { cardinality = Many; _ } ->
      Error (`Bad_query "stored-FK traversal currently supports to-one edges")
  | Some edge -> (
      match edge.storage_key with
      | Some storage_key -> Ok storage_key
      | None ->
          Error
            (`Bad_query
              ("edge " ^ edge.name ^ " does not have a stored foreign-key field")))

let foreign_key_value storage_key doc =
  match Bson.get_element storage_key doc with
  | exception Not_found -> Ok None
  | element -> (
      match value_of_bson_element element with
      | Error _ as error -> error
      | Ok Ent_ocaml.V_null -> Ok None
      | Ok value -> Ok (Some value))

let collect_foreign_keys storage_key docs =
  let rec loop acc = function
    | [] -> Ok (List.rev acc)
    | doc :: rest -> (
        match foreign_key_value storage_key doc with
        | Error _ as error -> error
        | Ok None -> loop acc rest
        | Ok (Some value) ->
            let acc = if List.mem value acc then acc else value :: acc in
            loop acc rest)
  in
  loop [] docs

let find_traversal_targets ctx edge_query ids =
  match ids with
  | [] -> Ok []
  | ids ->
      let target_query =
        edge_query.Ent_ocaml.target_query
        |> Ent_ocaml.Query.where (Ent_ocaml.In ("id", ids))
      in
      find ctx target_query

let traverse_as ctx (edge_query : Ent_ocaml.edge_query) ~decode =
  match stored_to_one_edge edge_query with
  | Error _ as error -> error
  | Ok storage_key -> (
      let source_query = { edge_query.source with Ent_ocaml.select = [] } in
      match find ctx source_query with
      | Error _ as error -> error
      | Ok source_docs -> (
          match collect_foreign_keys storage_key source_docs with
          | Error _ as error -> error
          | Ok ids -> (
              match find_traversal_targets ctx edge_query ids with
              | Error _ as error -> error
              | Ok target_docs -> decode_documents ~decode target_docs)))

let load_edge_as ctx (edge_query : Ent_ocaml.edge_query) ~decode_source
    ~decode_target =
  match stored_to_one_edge edge_query with
  | Error _ as error -> error
  | Ok storage_key -> (
      let source_query = { edge_query.source with Ent_ocaml.select = [] } in
      match find ctx source_query with
      | Error _ as error -> error
      | Ok source_docs -> (
          match collect_foreign_keys storage_key source_docs with
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
                            foreign_key_value storage_key source_doc )
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

let run_aggregate ctx (query : Ent_ocaml.query) pipeline =
  match
    Mongo_eio.direct_run_command ctx.client ctx.config.database
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
  match
    Mongo_eio.direct_insert_one ctx.client ~db:ctx.config.database
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
      match
        Mongo_eio.direct_insert_many ctx.client ~db:ctx.config.database
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
          let run =
            match mutation.op with
            | Ent_ocaml.Update_one ->
                Mongo_eio.direct_update_one ctx.client ~db:ctx.config.database
                  ~collection:entity.collection ~upsert:false selector
                  update_doc
            | Update ->
                Mongo_eio.direct_update_many ctx.client ~db:ctx.config.database
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
          match
            Mongo_eio.direct_update_one ctx.client ~db:ctx.config.database
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
              match
                Mongo_eio.direct_update_one ctx.client ~db:ctx.config.database
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
          let run =
            match mutation.op with
            | Ent_ocaml.Delete_one ->
                Mongo_eio.direct_delete_one ctx.client ~db:ctx.config.database
                  ~collection:entity.collection selector
            | Delete ->
                Mongo_eio.direct_delete_many ctx.client ~db:ctx.config.database
                  ~collection:entity.collection selector
            | Create | Update_one | Update | Upsert_one -> assert false
          in
          match run with
          | Ok result -> Ok result.Mongo_crud.deleted_count
          | Error error -> Error (backend_error "delete" entity error))))

let transaction ctx f = f ctx
