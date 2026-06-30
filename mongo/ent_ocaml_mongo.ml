type config = {
  database : string;
}

type ctx = {
  client : Mongo_eio.direct_client;
  config : config;
}

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
      Ok
        (doc
           [
             ( field,
               Bson.create_doc_element
                 (doc [ ("$regex", Bson.create_string (".*" ^ value ^ ".*")) ])
             );
           ])
  | Has_prefix (field, value) ->
      Ok
        (doc
           [
             ( field,
               Bson.create_doc_element
                 (doc [ ("$regex", Bson.create_string ("^" ^ value)) ]) );
           ])
  | Has_suffix (field, value) ->
      Ok
        (doc
           [
             ( field,
               Bson.create_doc_element
                 (doc [ ("$regex", Bson.create_string (value ^ "$")) ]) );
           ])
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

let filter_to_bson (query : Ent_ocaml.query) =
  match query.Ent_ocaml.predicates with
  | [] -> Ok Bson.empty
  | [ predicate ] -> predicate_to_bson predicate
  | predicates -> predicate_to_bson (And predicates)

let sort_to_bson orders =
  let direction = function
    | Ent_ocaml.Asc -> Bson.create_int32 1l
    | Desc -> Bson.create_int32 (-1l)
  in
  match orders with
  | [] -> None
  | orders ->
      orders
      |> List.map (fun order -> (order.Ent_ocaml.field, direction order.direction))
      |> doc |> Option.some

let field_storage_key ?(missing = "field not found: ") (entity : Ent_ocaml.entity)
    name =
  match List.find_opt (fun (field : Ent_ocaml.field) -> field.name = name) entity.fields with
  | Some field -> Ok field.storage_key
  | None -> Error (`Bad_schema (missing ^ name))

let projection_to_bson (query : Ent_ocaml.query) =
  match query.select with
  | [] -> Ok None
  | fields ->
      let rec loop acc = function
        | [] -> Ok (Some (doc (List.rev acc)))
        | field :: rest -> (
            match field_storage_key query.entity field with
            | Ok key -> loop ((key, Bson.create_int32 1l) :: acc) rest
            | Error _ as error -> error)
      in
      loop [] fields

let find_options (query : Ent_ocaml.query) =
  match (filter_to_bson query, projection_to_bson query) with
  | Error _ as error, _ | _, (Error _ as error) -> error
  | Ok filter, Ok projection ->
      Ok
         {
           (Mongo_crud.default_find query.Ent_ocaml.entity.collection filter) with
           projection;
           sort = sort_to_bson query.orders;
           skip = query.offset;
           limit = query.limit;
         }

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

let ensure_index ctx (entity : Ent_ocaml.entity) (index : Ent_ocaml.index) =
  match index_storage_fields entity index with
  | Error _ as error -> error
  | Ok [] -> Error (`Bad_schema "index has no fields")
  | Ok fields ->
      let options =
        (if index.unique then [ Mongo_index.Unique true ] else [])
        @
        match index.name with
        | None -> []
        | Some name -> [ Mongo_index.Name name ]
      in
      (match
         Mongo_eio.direct_ensure_index ctx.client ~db:ctx.config.database
           ~collection:entity.collection (index_key_bson fields) options
       with
      | Ok () -> Ok ()
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

let document_to_bson fields =
  let rec loop doc = function
    | [] -> Ok doc
    | (name, value) :: rest -> (
        match value_to_bson value with
        | Ok bson -> loop (Bson.add_element name bson doc) rest
        | Error _ as error -> error)
  in
  loop Bson.empty fields

let update_to_bson (mutation : Ent_ocaml.mutation) =
  let open Ent_ocaml in
  let add_if_any name doc update =
    if doc = Bson.empty then update
    else Bson.add_element name (Bson.create_doc_element doc) update
  in
  match (document_to_bson mutation.set, document_to_bson mutation.add) with
  | Error _ as error, _ | _, (Error _ as error) -> error
  | Ok set_doc, Ok inc_doc ->
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
      in
      if update = Bson.empty then bad_query "mutation has no update operations"
      else Ok update

let selector (mutation : Ent_ocaml.mutation) =
  match mutation.Ent_ocaml.predicates with
  | [] -> Ok Bson.empty
  | [ predicate ] -> predicate_to_bson predicate
  | predicates -> predicate_to_bson (And predicates)

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
          match document_to_bson mutation.set with
          | Error _ as error -> error
          | Ok doc -> insert ctx mutation.entity doc))
  | Update_one | Update | Delete_one | Delete ->
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
                  match document_to_bson mutation.set with
                  | Error _ as error -> error
                  | Ok doc ->
                      loop (Some mutation.entity) (doc :: acc) rest)
        | Update_one | Update | Delete_one | Delete ->
            Error (`Bad_query "insert_many_values expects Create mutation ops"))
  in
  loop None [] mutations

let update ctx (mutation : Ent_ocaml.mutation) =
  let entity = mutation.Ent_ocaml.entity in
  match mutation.op with
  | Create | Delete_one | Delete ->
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
            | Create | Delete_one | Delete -> assert false
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
  | Create | Update | Delete_one | Delete ->
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

let delete ctx (mutation : Ent_ocaml.mutation) =
  let entity = mutation.Ent_ocaml.entity in
  match mutation.op with
  | Create | Update_one | Update ->
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
            | Create | Update_one | Update -> assert false
          in
          match run with
          | Ok result -> Ok result.Mongo_crud.deleted_count
          | Error error -> Error (backend_error "delete" entity error))))
