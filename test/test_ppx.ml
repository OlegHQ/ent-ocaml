type post = {
  id : string [@ent.key "_id"] [@ent.unique] [@ent.immutable];
  user_id : string [@ent.index "posts_by_user"];
  body : string;
  media_ids : string list;
  status : string [@ent.enum [ "draft"; "published" ]];
  created_at_ms : int64;
  published_at_ms : int64 option;
}
[@@ent.entity "Post"] [@@ent.collection "posts"] [@@deriving ent]

type publish_state = {
  kind : string;
  external_id : string option;
}
[@@ent.entity "PublishState"] [@@ent.collection "publish_states"]
[@@deriving ent]

type publish_attempt = {
  id : string;
  state : publish_state;
}
[@@ent.entity "PublishAttempt"] [@@ent.collection "publish_attempts"]
[@@deriving ent]

let find_field name =
  List.find
    (fun (field : Ent_ocaml.field) -> field.name = name)
    post_entity.fields

let test_entity_metadata () =
  Alcotest.(check string) "entity name" "Post" post_entity.name;
  Alcotest.(check string) "collection" "posts" post_entity.collection;
  Alcotest.(check int) "field count" 7 (List.length post_entity.fields);
  Alcotest.(check int) "index count" 2 (List.length post_entity.indexes);
  let id = find_field "id" in
  Alcotest.(check string) "id storage key" "_id" id.storage_key;
  Alcotest.(check bool) "id unique" true id.unique;
  Alcotest.(check bool) "id immutable" true id.immutable;
  Alcotest.(check bool)
    "unique id index" true
    (List.exists
       (fun (index : Ent_ocaml.index) ->
         index.name = Some "unique_posts_id" && index.fields = [ "id" ]
         && index.unique)
       post_entity.indexes);
  Alcotest.(check bool)
    "named user index" true
    (List.exists
       (fun (index : Ent_ocaml.index) ->
         index.name = Some "posts_by_user" && index.fields = [ "user_id" ]
         && not index.unique)
       post_entity.indexes);
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
          Post.or_
            [
              Post.status_eq "draft";
              Post.not_ (Post.body_has_prefix "archived");
            ];
          Post.created_at_ms_gte 1_700_000_000L;
          Post.body_contains "hello";
          Post.media_ids_eq [ "media_1"; "media_2" ];
          Post.published_at_ms_is_nil ();
        ]
      ~order:[ Post.published_at_ms_order ~direction:Ent_ocaml.Desc () ]
      ~limit:10 ()
  in
  Alcotest.(check string) "entity" "Post" query.entity.name;
  Alcotest.(check int) "predicates" 6 (List.length query.predicates);
  Alcotest.(check int) "orders" 1 (List.length query.orders);
  Alcotest.(check (option int)) "limit" (Some 10) query.limit;
  Alcotest.(check bool)
    "first predicate" true
    (match List.hd query.predicates with
    | Ent_ocaml.Eq ("user_id", V_string "user_1") -> true
    | _ -> false)

let test_generated_mutation_api () =
  let create =
    Post.create
      [
        Post.id "post_1";
        Post.user_id "user_1";
        Post.body "hello";
        Post.media_ids [ "media_1"; "media_2" ];
        Post.status "draft";
        Post.created_at_ms 1_700_000_000L;
        Post.published_at_ms None;
      ]
  in
  Alcotest.(check bool)
    "create op" true
    (match create.op with Ent_ocaml.Create -> true | _ -> false);
  Alcotest.(check int) "create fields" 7 (List.length create.set);
  let update =
    Post.update_one ~where:[ Post.id_eq "post_1" ]
      ~set:[ Post.body "updated" ] ~clear:[ "published_at_ms" ] ()
  in
  Alcotest.(check bool)
    "update one op" true
    (match update.op with Ent_ocaml.Update_one -> true | _ -> false);
  Alcotest.(check int) "update predicates" 1 (List.length update.predicates);
  Alcotest.(check int) "update set" 1 (List.length update.set);
  Alcotest.(check int) "update clear" 1 (List.length update.clear);
  let delete = Post.delete_one ~where:[ Post.id_eq "post_1" ] () in
  Alcotest.(check bool)
    "delete one op" true
    (match delete.op with Ent_ocaml.Delete_one -> true | _ -> false)

let test_generated_nested_value_api () =
  let state = { kind = "confirmed"; external_id = Some "ext_1" } in
  let value = publish_state_to_ent_value state in
  Alcotest.(check bool)
    "state value" true
    (match value with
    | Ent_ocaml.V_doc
        [
          ("kind", V_string "confirmed");
          ("external_id", V_string "ext_1");
        ] ->
        true
    | _ -> false);
  let create =
    PublishAttempt.create
      [
        PublishAttempt.id "attempt_1";
        PublishAttempt.state state;
      ]
  in
  Alcotest.(check bool)
    "nested create" true
    (match create.set with
    | [ ("id", Ent_ocaml.V_string "attempt_1"); ("state", V_doc _) ] -> true
    | _ -> false)

let () =
  Alcotest.run "ent-ocaml-ppx"
    [
      ( "deriving",
        [
          Alcotest.test_case "entity metadata" `Quick test_entity_metadata;
          Alcotest.test_case "generated query api" `Quick
            test_generated_query_api;
          Alcotest.test_case "generated mutation api" `Quick
            test_generated_mutation_api;
          Alcotest.test_case "generated nested value api" `Quick
            test_generated_nested_value_api;
        ] );
    ]
