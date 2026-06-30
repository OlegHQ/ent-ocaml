let post_entity =
  Ent_ocaml.
    {
      name = "Post";
      collection = "posts";
      fields = [];
      edges = [];
      indexes = [];
    }

let query ?(predicates = []) ?(orders = []) ?limit ?offset () =
  Ent_ocaml.{ entity = post_entity; predicates; orders; limit; offset }

let mutation ?(predicates = []) ?(set = []) ?(clear = []) ?(add = []) op =
  Ent_ocaml.{ entity = post_entity; op; predicates; set; clear; add }

let test_error_to_string () =
  Alcotest.(check string)
    "not found" "not found" (Ent_ocaml.error_to_string `Not_found)

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

let () =
  Alcotest.run "ent-ocaml"
    [
      ("core", [ Alcotest.test_case "error strings" `Quick test_error_to_string ]);
      ( "mongo",
        [
          Alcotest.test_case "eq predicate bson" `Quick test_mongo_eq_predicate;
          Alcotest.test_case "filter planning" `Quick test_mongo_filter_planning;
          Alcotest.test_case "sort planning" `Quick test_mongo_sort_planning;
          Alcotest.test_case "update planning" `Quick test_mongo_update_planning;
          Alcotest.test_case "document planning" `Quick
            test_mongo_document_planning;
        ]
      );
    ]
