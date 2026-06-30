type post = {
  id : string [@ent.key "_id"] [@ent.unique] [@ent.immutable];
  user_id : string [@ent.index "posts_by_user"];
  body : string
  [@ent.validate
    [
      (fun value -> if value = "" then Error "must not be empty" else Ok ());
    ]];
  media_ids : string list;
  status : string [@ent.enum [ "draft"; "published" ]] [@ent.default "draft"];
  created_at_ms : int64;
  updated_at_ms : int64 [@ent.update_default 42L];
  published_at_ms : int64 option;
}
[@@ent.entity "Post"] [@@ent.collection "posts"]
[@@ent.indexes
  [
    {
      name = "posts_by_user_created";
      fields = [ "user_id"; "created_at_ms" ];
      unique = false;
    };
  ]]
[@@ent.edges
  [
    {
      name = "user";
      target = "User";
      storage_key = "user_id";
      cardinality = "one";
      required = true;
    };
  ]]
[@@deriving ent]

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
  Alcotest.(check int) "field count" 8 (List.length post_entity.fields);
  Alcotest.(check int) "index count" 3 (List.length post_entity.indexes);
  Alcotest.(check int) "edge count" 1 (List.length post_entity.edges);
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
  Alcotest.(check bool)
    "compound user created index" true
    (List.exists
       (fun (index : Ent_ocaml.index) ->
         index.name = Some "posts_by_user_created"
         && index.fields = [ "user_id"; "created_at_ms" ]
         && not index.unique)
       post_entity.indexes);
  Alcotest.(check bool)
    "user edge" true
    (List.exists
       (fun (edge : Ent_ocaml.edge) ->
         edge.name = "user" && edge.target = "User"
         && edge.storage_key = Some "user_id" && edge.required)
       post_entity.edges);
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
    Post.query ()
    |> Post.where (Post.user_id_eq "user_1")
    |> Post.where
         (Post.or_
            [
              Post.status_eq "draft";
              Post.not_ (Post.body_has_prefix "archived");
            ])
    |> Post.where (Post.created_at_ms_gte 1_700_000_000L)
    |> Post.where (Post.body_contains "hello")
    |> Post.where (Post.media_ids_eq [ "media_1"; "media_2" ])
    |> Post.where (Post.published_at_ms_is_nil ())
    |> Post.where (Post.user (Ent_ocaml.Eq ("id", Ent_ocaml.V_string "user_1")))
    |> Post.select [ Post.select_id; Post.select_body ]
    |> Post.order_by [ Post.published_at_ms_order ~direction:Ent_ocaml.Desc () ]
    |> Post.limit 10
  in
  Alcotest.(check string) "entity" "Post" query.entity.name;
  Alcotest.(check int) "predicates" 7 (List.length query.predicates);
  Alcotest.(check (list string)) "select" [ "id"; "body" ] query.select;
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
        Post.updated_at_ms 1_700_000_000L;
        Post.published_at_ms None;
      ]
  in
  Alcotest.(check bool)
    "create op" true
    (match create.op with Ent_ocaml.Create -> true | _ -> false);
  Alcotest.(check int) "create fields" 8 (List.length create.set);
  let create_many =
    Post.create_many
      [
        [
          Post.id "post_1";
          Post.user_id "user_1";
          Post.body "hello";
          Post.media_ids [];
          Post.status "draft";
          Post.created_at_ms 1L;
          Post.updated_at_ms 1L;
          Post.published_at_ms None;
        ];
        [
          Post.id "post_2";
          Post.user_id "user_1";
          Post.body "second";
          Post.media_ids [];
          Post.status "draft";
          Post.created_at_ms 2L;
          Post.updated_at_ms 2L;
          Post.published_at_ms None;
        ];
      ]
  in
  Alcotest.(check int) "bulk create rows" 2 (List.length create_many);
  Alcotest.(check bool)
    "bulk create ops" true
    (List.for_all
       (fun mutation ->
         match mutation.Ent_ocaml.op with Ent_ocaml.Create -> true | _ -> false)
       create_many);
  let create_with_default =
    Post.create
      [
        Post.id "post_3";
        Post.user_id "user_1";
        Post.body "default status";
        Post.media_ids [];
        Post.created_at_ms 3L;
        Post.updated_at_ms 3L;
        Post.published_at_ms None;
      ]
  in
  Alcotest.(check bool)
    "default status" true
    (List.exists
       (function
         | "status", Ent_ocaml.V_string "draft" -> true
         | _ -> false)
       create_with_default.set);
  (match Ent_ocaml.validate_mutation create_with_default with
  | Ok () -> ()
  | Error error -> Alcotest.fail (Ent_ocaml.error_to_string error));
  let create_invalid =
    Post.create
      [
        Post.id "post_invalid";
        Post.user_id "user_1";
        Post.body "";
        Post.media_ids [];
        Post.created_at_ms 4L;
        Post.updated_at_ms 4L;
        Post.published_at_ms None;
      ]
  in
  (match Ent_ocaml.validate_mutation create_invalid with
  | Ok () -> Alcotest.fail "expected generated validator error"
  | Error (`Bad_query message) ->
      Alcotest.(check string)
        "validator message"
        "validation failed for field body: must not be empty"
        message
  | Error error -> Alcotest.fail (Ent_ocaml.error_to_string error));
  let update =
    Post.update_one ~where:[ Post.id_eq "post_1" ]
      ~set:[ Post.body "updated" ] ~clear:[ "published_at_ms" ] ()
  in
  Alcotest.(check bool)
    "update one op" true
    (match update.op with Ent_ocaml.Update_one -> true | _ -> false);
  Alcotest.(check int) "update predicates" 1 (List.length update.predicates);
  Alcotest.(check int) "update set" 2 (List.length update.set);
  Alcotest.(check bool)
    "update default" true
    (List.exists
       (function
         | "updated_at_ms", Ent_ocaml.V_int64 42L -> true
         | _ -> false)
       update.set);
  Alcotest.(check int) "update clear" 1 (List.length update.clear);
  let clear_updated =
    Post.update_one ~where:[ Post.id_eq "post_1" ]
      ~clear:[ "updated_at_ms" ] ()
  in
  Alcotest.(check bool)
    "cleared update default omitted" false
    (List.exists
       (function
         | "updated_at_ms", Ent_ocaml.V_int64 42L -> true
         | _ -> false)
       clear_updated.set);
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
