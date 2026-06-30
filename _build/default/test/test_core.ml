let post_entity =
  Ent_ocaml.
    {
      name = "Post";
      collection = "posts";
      fields = [];
      edges = [];
      indexes = [];
    }

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

let () =
  Alcotest.run "ent-ocaml"
    [
      ("core", [ Alcotest.test_case "error strings" `Quick test_error_to_string ]);
      ( "mongo",
        [ Alcotest.test_case "eq predicate bson" `Quick test_mongo_eq_predicate ]
      );
    ]
