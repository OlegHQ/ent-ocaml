type post = {
  id : string [@ent.key "_id"] [@ent.unique] [@ent.immutable];
  user_id : string;
  body : string;
  media_ids : string list;
  status : string [@ent.enum [ "draft"; "published" ]];
  published_at_ms : int64 option;
}
[@@ent.entity "Post"] [@@ent.collection "posts"] [@@deriving ent]

let find_field name =
  List.find
    (fun (field : Ent_ocaml.field) -> field.name = name)
    post_entity.fields

let test_entity_metadata () =
  Alcotest.(check string) "entity name" "Post" post_entity.name;
  Alcotest.(check string) "collection" "posts" post_entity.collection;
  Alcotest.(check int) "field count" 6 (List.length post_entity.fields);
  let id = find_field "id" in
  Alcotest.(check string) "id storage key" "_id" id.storage_key;
  Alcotest.(check bool) "id unique" true id.unique;
  Alcotest.(check bool) "id immutable" true id.immutable;
  let published_at = find_field "published_at_ms" in
  Alcotest.(check bool) "option is not required" false published_at.required;
  Alcotest.(check bool) "option is nillable" true published_at.nillable;
  let status = find_field "status" in
  Alcotest.(check bool)
    "enum type" true
    (match status.typ with
    | Ent_ocaml.Enum [ "draft"; "published" ] -> true
    | _ -> false)

let test_generated_query_api () =
  let query =
    Post.query
      ~where:
        [
          Post.user_id_eq "user_1";
          Post.body_contains "hello";
          Post.published_at_ms_is_nil ();
        ]
      ~order:[ Post.published_at_ms_order ~direction:Ent_ocaml.Desc () ]
      ~limit:10 ()
  in
  Alcotest.(check string) "entity" "Post" query.entity.name;
  Alcotest.(check int) "predicates" 3 (List.length query.predicates);
  Alcotest.(check int) "orders" 1 (List.length query.orders);
  Alcotest.(check (option int)) "limit" (Some 10) query.limit;
  Alcotest.(check bool)
    "first predicate" true
    (match List.hd query.predicates with
    | Ent_ocaml.Eq ("user_id", V_string "user_1") -> true
    | _ -> false)

let () =
  Alcotest.run "ent-ocaml-ppx"
    [
      ( "deriving",
        [
          Alcotest.test_case "entity metadata" `Quick test_entity_metadata;
          Alcotest.test_case "generated query api" `Quick
            test_generated_query_api;
        ] );
    ]
