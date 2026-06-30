let post_entity =
  Ent_ocaml.
    {
      name = "Post";
      collection = "posts";
      fields =
        [
          {
            name = "id";
            storage_key = "_id";
            typ = String;
            required = true;
            unique = true;
            immutable = true;
            nillable = false;
          };
          {
            name = "user_id";
            storage_key = "user_id";
            typ = String;
            required = true;
            unique = false;
            immutable = false;
            nillable = false;
          };
        ];
      edges = [];
      indexes = [];
    }

let query ?(predicates = []) ?(select = []) ?(orders = []) ?limit ?offset () =
  Ent_ocaml.{ entity = post_entity; predicates; select; orders; limit; offset }

let mutation ?(predicates = []) ?(set = []) ?(clear = []) ?(add = []) op =
  Ent_ocaml.{ entity = post_entity; op; predicates; set; clear; add }

let test_error_to_string () =
  Alcotest.(check string)
    "not found" "not found" (Ent_ocaml.error_to_string `Not_found)

let test_validate_create_missing_required () =
  match
    Ent_ocaml.validate_mutation
      (mutation Ent_ocaml.Create
         ~set:Ent_ocaml.[ ("id", V_string "post_1") ])
  with
  | Ok () -> Alcotest.fail "expected missing required field"
  | Error (`Bad_query message) ->
      Alcotest.(check string)
        "message" "missing required field: user_id" message
  | Error error -> Alcotest.fail (Ent_ocaml.error_to_string error)

let test_validate_unknown_field () =
  match
    Ent_ocaml.validate_mutation
      (mutation Ent_ocaml.Create
         ~set:
           Ent_ocaml.
             [
               ("id", V_string "post_1");
               ("user_id", V_string "user_1");
               ("missing", V_string "bad");
             ])
  with
  | Ok () -> Alcotest.fail "expected unknown field"
  | Error (`Bad_query message) ->
      Alcotest.(check string)
        "message" "mutation field not found: missing" message
  | Error error -> Alcotest.fail (Ent_ocaml.error_to_string error)

let test_validate_immutable_update () =
  match
    Ent_ocaml.validate_mutation
      (mutation Ent_ocaml.Update_one
         ~set:Ent_ocaml.[ ("id", V_string "post_2") ])
  with
  | Ok () -> Alcotest.fail "expected immutable update error"
  | Error (`Bad_query message) ->
      Alcotest.(check string)
        "message" "immutable field cannot be updated: id" message
  | Error error -> Alcotest.fail (Ent_ocaml.error_to_string error)

let test_mongo_eq_predicate () =
  match
    Ent_ocaml_mongo.predicate_to_bson
      Ent_ocaml.(Eq ("user_id", V_string "user_1"))
  with
  | Error error -> Alcotest.fail (Ent_ocaml.error_to_string error)
  | Ok bson ->
      Alcotest.(check string)
        "field" "user_1"
        (Bson.get_string (Bson.get_element "user_id" bson))

let test_mongo_filter_planning () =
  let filter =
    match
      Ent_ocaml_mongo.filter_to_bson
        (query
           ~predicates:
             Ent_ocaml.
               [
                 Eq ("user_id", V_string "user_1");
                 Or [ Eq ("status", V_string "draft"); Is_nil "published_at" ];
               ]
           ())
    with
    | Ok filter -> filter
    | Error error -> Alcotest.fail (Ent_ocaml.error_to_string error)
  in
  let clauses = Bson.get_list (Bson.get_element "$and" filter) in
  Alcotest.(check int) "and clauses" 2 (List.length clauses)

let test_mongo_sort_planning () =
  let sort =
    Ent_ocaml_mongo.sort_to_bson
      Ent_ocaml.[ { field = "created_at_ms"; direction = Desc } ]
    |> Option.get
  in
  Alcotest.(check int32)
    "descending sort" (-1l)
    (Bson.get_int32 (Bson.get_element "created_at_ms" sort))

let test_mongo_projection_planning () =
  let projection =
    match Ent_ocaml_mongo.projection_to_bson (query ~select:[ "id" ] ()) with
    | Ok (Some projection) -> projection
    | Ok None -> Alcotest.fail "expected projection"
    | Error error -> Alcotest.fail (Ent_ocaml.error_to_string error)
  in
  Alcotest.(check int32)
    "id storage projection" 1l
    (Bson.get_int32 (Bson.get_element "_id" projection))

let test_mongo_projection_missing_field () =
  match Ent_ocaml_mongo.projection_to_bson (query ~select:[ "missing" ] ()) with
  | Ok _ -> Alcotest.fail "expected missing projection field error"
  | Error (`Bad_schema message) ->
      Alcotest.(check string) "message" "field not found: missing" message
  | Error error -> Alcotest.fail (Ent_ocaml.error_to_string error)

let test_mongo_update_planning () =
  let update =
    match
      Ent_ocaml_mongo.update_to_bson
        (mutation Ent_ocaml.Update_one
           ~set:Ent_ocaml.[ ("body", V_string "updated") ]
           ~clear:[ "published_at_ms" ]
           ~add:Ent_ocaml.[ ("revision", V_int 1) ])
    with
    | Ok update -> update
    | Error error -> Alcotest.fail (Ent_ocaml.error_to_string error)
  in
  let set = Bson.get_doc_element (Bson.get_element "$set" update) in
  let unset = Bson.get_doc_element (Bson.get_element "$unset" update) in
  let inc = Bson.get_doc_element (Bson.get_element "$inc" update) in
  Alcotest.(check string)
    "set body" "updated" (Bson.get_string (Bson.get_element "body" set));
  Alcotest.(check string)
    "unset field" "" (Bson.get_string (Bson.get_element "published_at_ms" unset));
  Alcotest.(check int64)
    "increment" 1L (Bson.get_int64 (Bson.get_element "revision" inc))

let test_mongo_document_planning () =
  let doc =
    match
      Ent_ocaml_mongo.document_to_bson
        Ent_ocaml.
          [
            ("id", V_string "post_1");
            ("media_ids", V_list [ V_string "media_1"; V_string "media_2" ]);
            ("published_at_ms", V_null);
          ]
    with
    | Ok doc -> doc
    | Error error -> Alcotest.fail (Ent_ocaml.error_to_string error)
  in
  Alcotest.(check string)
    "id" "post_1" (Bson.get_string (Bson.get_element "id" doc));
  Alcotest.(check int)
    "media count" 2
    (Bson.get_list (Bson.get_element "media_ids" doc) |> List.length);
  ignore (Bson.get_null (Bson.get_element "published_at_ms" doc))

let test_mongo_decode_documents () =
  let doc value = Bson.add_element "name" (Bson.create_string value) Bson.empty in
  let decode doc =
    try Ok (Bson.get_string (Bson.get_element "name" doc)) with
    | _ -> Error "missing name"
  in
  match Ent_ocaml_mongo.decode_documents ~decode [ doc "a"; doc "b" ] with
  | Error error -> Alcotest.fail (Ent_ocaml.error_to_string error)
  | Ok values -> Alcotest.(check (list string)) "decoded" [ "a"; "b" ] values

let test_mongo_decode_error () =
  let decode _ = Error "bad document" in
  match Ent_ocaml_mongo.decode_document ~decode Bson.empty with
  | Ok _ -> Alcotest.fail "expected decode error"
  | Error (`Decode message) ->
      Alcotest.(check string) "decode message" "bad document" message
  | Error error -> Alcotest.fail (Ent_ocaml.error_to_string error)

let test_mongo_index_storage_fields () =
  let index =
    Ent_ocaml.
      { name = Some "unique_posts_id"; fields = [ "id" ]; edges = []; unique = true }
  in
  match Ent_ocaml_mongo.index_storage_fields post_entity index with
  | Error error -> Alcotest.fail (Ent_ocaml.error_to_string error)
  | Ok fields -> Alcotest.(check (list string)) "storage fields" [ "_id" ] fields

let test_mongo_index_missing_field () =
  let index =
    Ent_ocaml.
      { name = Some "bad"; fields = [ "missing" ]; edges = []; unique = false }
  in
  match Ent_ocaml_mongo.index_storage_fields post_entity index with
  | Ok _ -> Alcotest.fail "expected missing field error"
  | Error (`Bad_schema message) ->
      Alcotest.(check string) "message" "index field not found: missing" message
  | Error error -> Alcotest.fail (Ent_ocaml.error_to_string error)

let () =
  Alcotest.run "ent-ocaml"
    [
      ( "core",
        [
          Alcotest.test_case "error strings" `Quick test_error_to_string;
          Alcotest.test_case "validate create required" `Quick
            test_validate_create_missing_required;
          Alcotest.test_case "validate unknown field" `Quick
            test_validate_unknown_field;
          Alcotest.test_case "validate immutable update" `Quick
            test_validate_immutable_update;
        ] );
      ( "mongo",
        [
          Alcotest.test_case "eq predicate bson" `Quick test_mongo_eq_predicate;
          Alcotest.test_case "filter planning" `Quick test_mongo_filter_planning;
          Alcotest.test_case "sort planning" `Quick test_mongo_sort_planning;
          Alcotest.test_case "projection planning" `Quick
            test_mongo_projection_planning;
          Alcotest.test_case "projection missing field" `Quick
            test_mongo_projection_missing_field;
          Alcotest.test_case "update planning" `Quick test_mongo_update_planning;
          Alcotest.test_case "document planning" `Quick
            test_mongo_document_planning;
          Alcotest.test_case "decode documents" `Quick
            test_mongo_decode_documents;
          Alcotest.test_case "decode error" `Quick test_mongo_decode_error;
          Alcotest.test_case "index storage fields" `Quick
            test_mongo_index_storage_fields;
          Alcotest.test_case "index missing field" `Quick
            test_mongo_index_missing_field;
        ]
      );
    ]
