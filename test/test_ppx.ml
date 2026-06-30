type user = {
  id : string [@ent.key "_id"] [@ent.unique] [@ent.immutable];
  username : string [@ent.unique] [@ent.index "unique_username"];
}
[@@ent.entity "User"] [@@ent.collection "users"]
[@@deriving ent]

type post = {
  id : string [@ent.key "_id"] [@ent.unique] [@ent.immutable];
  user_id : string [@ent.index "posts_by_user"];
  body : string
  [@ent.sensitive]
  [@ent.comment "Post body text"]
  [@ent.deprecated "use summary"]
  [@ent.validate
    [
      (fun value -> if value = "" then Error "must not be empty" else Ok ());
    ]];
  media_ids : string list
  [@ent.validate
    [
      (fun values ->
        if List.length values > 2 then Error "too many media items" else Ok ());
    ]];
  status : string [@ent.enum [ "draft"; "published" ]] [@ent.default "draft"];
  created_at_ms : int64;
  updated_at_ms : int64 [@ent.update_default 42L];
  published_at_ms : int64 option
  [@ent.validate
    [
      (function
      | Some value when value < 0L -> Error "must not be negative"
      | Some _ | None -> Ok ());
    ]];
}
[@@ent.entity "Post"] [@@ent.collection "posts"]
[@@ent.indexes
  [
    {
      name = "posts_by_user_created";
      fields = [ "user_id"; "created_at_ms" ];
      unique = false;
      partial_filter =
        [ Ent_ocaml.Eq ("status", Ent_ocaml.V_string "published") ];
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
[@@ent.query_rules [ (fun _ctx _query -> Ent_ocaml.Deny "schema no reads") ]]
[@@ent.mutation_rules
  [ (fun _ctx _mutation -> Ent_ocaml.Deny "schema no writes") ]]
[@@ent.mutation_hooks
  [
    {
      Ent_ocaml.wrap_mutation =
        (fun next ctx mutation ->
          next ctx
            (Ent_ocaml.Mutation.set ("body", Ent_ocaml.V_string "schema hook")
               mutation));
    };
  ]]
[@@ent.query_interceptors
  [
    {
      Ent_ocaml.wrap_query =
        (fun next ctx query ->
          next ctx
            (query
             |> Ent_ocaml.Query.where
                  (Ent_ocaml.Eq
                     ("status", Ent_ocaml.V_string "schema interceptor"))));
    };
  ]]
[@@ent.edge_interceptors
  [
    {
      Ent_ocaml.wrap_edge =
        (fun next ctx edge_query ->
          next ctx
            {
              edge_query with
              Ent_ocaml.target = edge_query.Ent_ocaml.source.entity;
            });
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
  state : publish_state
  [@ent.validate
    [
      (fun state ->
        if state.kind = "" then Error "kind must not be empty" else Ok ());
    ]];
}
[@@ent.entity "PublishAttempt"] [@@ent.collection "publish_attempts"]
[@@deriving ent]

type event = {
  id : string;
  metadata : Ent_ocaml.value [@ent.json] [@ent.optional];
}
[@@ent.entity "Event"] [@@ent.collection "events"]
[@@deriving ent]

type task = {
  id : string;
  title : string;
  created_at_ms : int64 [@ent.default_result Ok 100L];
  updated_at_ms : int64 [@ent.update_default_result Ok 200L];
}
[@@ent.entity "Task"] [@@ent.collection "tasks"]
[@@deriving ent]

type failing_task = {
  id : string;
  title : string;
  created_at_ms : int64
  [@ent.default_result Error (`Bad_query "clock unavailable")];
}
[@@ent.entity "FailingTask"] [@@ent.collection "failing_tasks"]
[@@deriving ent]

module Memory_backend = struct
  type ctx = unit
  type doc = Ent_ocaml.value

  let find_as () (query : Ent_ocaml.query) ~decode =
    match decode (Ent_ocaml.V_string query.Ent_ocaml.entity.name) with
    | Ok value -> Ok [ value ]
    | Error message -> Error (`Decode message)

  let find_one_as () (query : Ent_ocaml.query) ~decode =
    match decode (Ent_ocaml.V_string query.Ent_ocaml.entity.name) with
    | Ok value -> Ok (Some value)
    | Error message -> Error (`Decode message)

  let values () (query : Ent_ocaml.query) =
    let order_values =
      query.Ent_ocaml.orders
      |> List.filter_map (fun ({ value_alias; _ } as order : Ent_ocaml.order) ->
             Option.map
               (fun alias -> (alias, Ent_ocaml.V_string (Ent_ocaml.Order.field_name order)))
               value_alias)
    in
    Ok
      [
        Ent_ocaml.V_doc
          (List.map
             (fun field -> (field, Ent_ocaml.V_string field))
             query.Ent_ocaml.select
          @ order_values);
      ]

  let value () (query : Ent_ocaml.query) =
    let order_values =
      query.Ent_ocaml.orders
      |> List.filter_map (fun ({ value_alias; _ } as order : Ent_ocaml.order) ->
             Option.map
               (fun _alias -> Ent_ocaml.Order.field_name order)
               value_alias)
    in
    match query.Ent_ocaml.select @ order_values with
    | [ field ] -> Ok (Some (Ent_ocaml.V_string field))
    | [] | _ :: _ :: _ ->
        Error (`Bad_query "value expects one selected field or order value")

  let traverse_as () (edge_query : Ent_ocaml.edge_query) ~decode =
    match decode (Ent_ocaml.V_string edge_query.Ent_ocaml.target.name) with
    | Ok value -> Ok [ value ]
    | Error message -> Error (`Decode message)

  let load_edge_as () (edge_query : Ent_ocaml.edge_query) ~decode_source
      ~decode_target =
    match
      ( decode_source
          (Ent_ocaml.V_string edge_query.Ent_ocaml.source.entity.name),
        decode_target (Ent_ocaml.V_string edge_query.target.name) )
    with
    | Ok source, Ok target -> Ok [ (source, Some target) ]
    | Error message, _ | _, Error message -> Error (`Decode message)

  let insert_values () (mutation : Ent_ocaml.mutation) =
    Ok (Ent_ocaml.V_doc mutation.set)

  let insert_many_values ?ordered:_ () mutations =
    Ok
      (List.map
         (fun (mutation : Ent_ocaml.mutation) ->
           Ent_ocaml.V_doc mutation.Ent_ocaml.set)
         mutations)
  let update_one () _mutation = Ok ()
  let update () _mutation = Ok 1
  let upsert_one () _mutation = Ok ()
  let delete () _mutation = Ok 1
  let count () (query : Ent_ocaml.query) =
    Ok (List.length query.Ent_ocaml.predicates)

  let aggregate () (aggregate : Ent_ocaml.aggregate) =
    match aggregate.op with
    | Ent_ocaml.Count -> Ok (Some (Ent_ocaml.V_int 2))
    | Ent_ocaml.Min _ | Ent_ocaml.Max _ | Ent_ocaml.Sum _ | Ent_ocaml.Avg _ ->
        Ok (Some (Ent_ocaml.V_int64 42L))

  let aggregate_scan () (scan : Ent_ocaml.aggregate_scan) =
    Ok
      (List.map
         (fun (name, op) ->
           match op with
           | Ent_ocaml.Count -> (name, Some (Ent_ocaml.V_int 2))
           | Min _ | Max _ | Sum _ | Avg _ ->
               (name, Some (Ent_ocaml.V_int64 42L)))
         scan.ops)

  let group () (group : Ent_ocaml.group_aggregate) =
    Ok
      [
        {
          Ent_ocaml.group = Ent_ocaml.V_string group.group;
          value = Some (V_int64 42L);
        };
      ]

  let transaction ?options:_ ctx f = f ctx
end

let find_field name =
  List.find
    (fun (field : Ent_ocaml.field) -> field.name = name)
    post_entity.fields

let test_entity_metadata () =
  Alcotest.(check string) "entity name" "Post" post_entity.name;
  Alcotest.(check string) "collection" "posts" post_entity.collection;
  Alcotest.(check int) "field count" 8 (List.length post_entity.fields);
  Alcotest.(check int) "index count" 2 (List.length post_entity.indexes);
  Alcotest.(check int) "edge count" 1 (List.length post_entity.edges);
  let id = find_field "id" in
  Alcotest.(check string) "id storage key" "_id" id.storage_key;
  Alcotest.(check bool) "id unique" true id.unique;
  Alcotest.(check bool) "id immutable" true id.immutable;
  Alcotest.(check bool)
    "id unique index is implicit" false
    (List.exists
       (fun (index : Ent_ocaml.index) -> index.fields = [ "id" ])
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
         && not index.unique
         && index.partial_filter
            = [ Ent_ocaml.Eq ("status", Ent_ocaml.V_string "published") ])
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
    | _ -> false);
  let body = find_field "body" in
  Alcotest.(check bool) "body sensitive" true body.sensitive;
  Alcotest.(check (option string))
    "body deprecated" (Some "use summary") body.deprecated;
  Alcotest.(check (option string))
    "body comment" (Some "Post body text") body.comment

let test_generated_schema_snapshot () =
  match post_schema_snapshot with
  | Ent_ocaml.V_doc fields ->
      Alcotest.(check (option string))
        "name" (Some "Post")
        (Option.map
           (function Ent_ocaml.V_string value -> value | _ -> "")
           (List.assoc_opt "name" fields));
      Alcotest.(check bool)
        "fields" true
        (match List.assoc_opt "fields" fields with
        | Some (Ent_ocaml.V_list fields) -> List.length fields = 8
        | _ -> false);
      Alcotest.(check bool)
        "field metadata" true
        (match List.assoc_opt "fields" fields with
        | Some (Ent_ocaml.V_list field_values) ->
            List.exists
              (function
                | Ent_ocaml.V_doc field ->
                    List.assoc_opt "name" field
                    = Some (Ent_ocaml.V_string "body")
                    && List.assoc_opt "sensitive" field
                       = Some (Ent_ocaml.V_bool true)
                    && List.assoc_opt "deprecated" field
                       = Some (Ent_ocaml.V_string "use summary")
                    && List.assoc_opt "comment" field
                       = Some (Ent_ocaml.V_string "Post body text")
                | _ -> false)
              field_values
        | _ -> false)
  | _ -> Alcotest.fail "expected generated schema snapshot document"

let test_generated_schema_manifest () =
  match
    Ent_ocaml.Schema_snapshot.manifest ~name:"test"
      [
        user_entity;
        post_entity;
        publish_state_entity;
        publish_attempt_entity;
        event_entity;
      ]
  with
  | Ent_ocaml.V_doc fields ->
      Alcotest.(check (option string))
        "name" (Some "test")
        (Option.map
           (function Ent_ocaml.V_string value -> value | _ -> "")
           (List.assoc_opt "name" fields));
      Alcotest.(check bool)
        "entities" true
        (match List.assoc_opt "entities" fields with
        | Some (Ent_ocaml.V_list entities) -> List.length entities = 5
        | _ -> false)
  | _ -> Alcotest.fail "expected generated schema manifest document"

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
  Alcotest.(check bool)
    "order value alias" true
    (match query.orders with
    | [
        {
          Ent_ocaml.target = Field_order "published_at_ms";
          value_alias = None;
          _;
        };
      ] ->
        true
    | _ -> false);
  Alcotest.(check (option int)) "limit" (Some 10) query.limit;
  Alcotest.(check bool)
    "first predicate" true
    (match List.hd query.predicates with
    | Ent_ocaml.Eq ("user_id", V_string "user_1") -> true
    | _ -> false);
  let aggregate =
    let open Post in
    query ()
    |> where (status_eq "draft")
    |> sum select_created_at_ms
  in
  Alcotest.(check bool)
    "aggregate sum" true
    (match aggregate.op with Ent_ocaml.Sum "created_at_ms" -> true | _ -> false);
  Alcotest.(check int)
    "aggregate predicates" 1
    (List.length aggregate.query.predicates)

let test_generated_order_value_api () =
  let query =
    let open Post in
    query ()
    |> order_by
         [ created_at_ms_order ~direction:Ent_ocaml.Desc ~as_:"created" () ]
  in
  Alcotest.(check bool)
    "order value alias" true
    (match query.orders with
    | [
        {
          Ent_ocaml.target = Field_order "created_at_ms";
          direction = Desc;
          value_alias = Some "created";
        };
      ] ->
        true
    | _ -> false);
  let module Posts = Post.Store (Memory_backend) in
  match Posts.values () query with
  | Ok [ Ent_ocaml.V_doc [ ("created", Ent_ocaml.V_string "created_at_ms") ] ] ->
      ()
  | Ok _ -> Alcotest.fail "unexpected order value result"
  | Error error -> Alcotest.fail (Ent_ocaml.error_to_string error)

let test_generated_edge_order_api () =
  let query =
    let open Post in
    query ()
    |> order_by
         [
           user_field_order ~target:user_entity ~direction:Ent_ocaml.Asc
             ~as_:"author_name" "username" ();
         ]
  in
  Alcotest.(check bool)
    "edge order target" true
    (match query.orders with
    | [
        {
          Ent_ocaml.target =
            Edge_field_order { edge = "user"; target; field = "username" };
          direction = Asc;
          value_alias = Some "author_name";
        };
      ] ->
        target.name = "User"
    | _ -> false);
  let module Posts = Post.Store (Memory_backend) in
  match Posts.value () query with
  | Ok (Some (Ent_ocaml.V_string "user.username")) -> ()
  | Ok _ -> Alcotest.fail "unexpected edge order value result"
  | Error error -> Alcotest.fail (Ent_ocaml.error_to_string error)

let test_generated_edge_count_order_api () =
  let query =
    let open Post in
    query ()
    |> order_by
         [
           user_count_order ~target:user_entity ~direction:Ent_ocaml.Desc
             ~as_:"user_count" ();
         ]
  in
  Alcotest.(check bool)
    "edge count order target" true
    (match query.orders with
    | [
        {
          Ent_ocaml.target = Edge_count_order { edge = "user"; target };
          direction = Desc;
          value_alias = Some "user_count";
        };
      ] ->
        target.name = "User"
    | _ -> false);
  let module Posts = Post.Store (Memory_backend) in
  match Posts.value () query with
  | Ok (Some (Ent_ocaml.V_string "user.count")) -> ()
  | Ok _ -> Alcotest.fail "unexpected edge count order value result"
  | Error error -> Alcotest.fail (Ent_ocaml.error_to_string error)

let test_generated_id_api () =
  let query =
    let open Post in
    by_id "post_1"
  in
  Alcotest.(check bool)
    "by id predicate" true
    (match query.predicates with
    | [ Ent_ocaml.Eq ("id", V_string "post_1") ] -> true
    | _ -> false);
  let update =
    let open Post in
    update_id "post_1" |> set (body "updated")
  in
  Alcotest.(check bool)
    "update id op" true
    (match update.op with Ent_ocaml.Update_one -> true | _ -> false);
  Alcotest.(check int) "update id predicates" 1
    (List.length update.predicates);
  let delete =
    let open Post in
    delete_id "post_1"
  in
  Alcotest.(check bool)
    "delete id op" true
    (match delete.op with Ent_ocaml.Delete_one -> true | _ -> false);
  Alcotest.(check int) "delete id predicates" 1
    (List.length delete.predicates)

let test_generated_cursor_api () =
  let query =
    let open Post in
    query ()
    |> after_created_at_ms ~direction:Ent_ocaml.Desc 1_700_000_100L
    |> limit 20
  in
  Alcotest.(check (option int)) "limit" (Some 20) query.limit;
  Alcotest.(check bool)
    "cursor predicate" true
    (match query.predicates with
    | [ Ent_ocaml.Lt ("created_at_ms", V_int64 1_700_000_100L) ] -> true
    | _ -> false);
  Alcotest.(check bool)
    "cursor order" true
    (match query.orders with
    | [ { Ent_ocaml.target = Field_order "created_at_ms"; direction = Desc } ] ->
        true
    | _ -> false)

let test_generated_composite_cursor_api () =
  let query =
    let open Post in
    query ()
    |> after_cursor
         [
           created_at_ms_cursor ~direction:Ent_ocaml.Desc 1_700_000_100L;
           id_cursor ~direction:Ent_ocaml.Desc "post_2";
         ]
    |> limit 20
  in
  Alcotest.(check (option int)) "limit" (Some 20) query.limit;
  Alcotest.(check int) "cursor predicates" 1 (List.length query.predicates);
  Alcotest.(check bool)
    "cursor predicate" true
    (match query.predicates with
    | [ Ent_ocaml.Or [ Lt ("created_at_ms", V_int64 _); And _ ] ] -> true
    | _ -> false);
  Alcotest.(check int) "cursor orders" 2 (List.length query.orders)

let test_generated_traversal_api () =
  let edge_query =
    let open Post in
    query ()
    |> where (status_eq "draft")
    |> with_user ~as_:"author" ~target:user_entity
  in
  Alcotest.(check string)
    "source entity" "Post" edge_query.Ent_ocaml.source.entity.name;
  Alcotest.(check string) "edge" "user" edge_query.edge;
  Alcotest.(check (option string))
    "edge alias" (Some "author") edge_query.edge_alias;
  Alcotest.(check string) "target entity" "User" edge_query.target.name;
  Alcotest.(check int)
    "source predicates" 1
    (List.length edge_query.source.predicates);
  let target_predicate =
    let open Post in
    user ~target:user_entity (User.username_eq "alice")
  in
  Alcotest.(check bool)
    "target edge predicate" true
    (match target_predicate with
    | Ent_ocaml.Has_edge_with_target
        {
          edge = "user";
          target;
          predicates = [ Ent_ocaml.Eq ("username", Ent_ocaml.V_string "alice") ];
        } ->
        target.name = "User"
    | _ -> false);
  let module Store = Post.Store (Memory_backend) in
  let decode = function
    | Ent_ocaml.V_string value -> Ok value
    | _ -> Error "expected string"
  in
  (match Store.traverse () ~decode edge_query with
  | Ok [ "User" ] -> ()
  | Ok _ -> Alcotest.fail "unexpected traverse result"
  | Error error -> Alcotest.fail (Ent_ocaml.error_to_string error));
  (match
     Store.load_edge () ~decode_source:decode ~decode_target:decode edge_query
   with
  | Ok [ ("Post", Some "User") ] -> ()
  | Ok _ -> Alcotest.fail "unexpected load_edge result"
  | Error error -> Alcotest.fail (Ent_ocaml.error_to_string error));
  (match
     Store.load_edge_named () ~decode_source:decode ~decode_target:decode
       edge_query
   with
  | Ok
      [
        {
          Ent_ocaml.loaded_edge = "user";
          loaded_alias = Some "author";
          loaded_name = "author";
          loaded_source = "Post";
          loaded_target = Some "User";
        };
      ] ->
      ()
  | Ok _ -> Alcotest.fail "unexpected named load_edge result"
  | Error error -> Alcotest.fail (Ent_ocaml.error_to_string error));
  let second_edge_query =
    let open Post in
    query () |> with_user ~as_:"editor" ~target:user_entity
  in
  (match
     Store.load_edges_named () ~decode_source:decode ~decode_target:decode
       [ edge_query; second_edge_query ]
   with
  | Ok
      [
        {
          Ent_ocaml.loaded_group_edge = "user";
          loaded_group_alias = Some "author";
          loaded_group_name = "author";
          loaded_group_rows =
            [
              {
                Ent_ocaml.loaded_edge = "user";
                loaded_alias = Some "author";
                loaded_name = "author";
                loaded_source = "Post";
                loaded_target = Some "User";
              };
            ];
        };
        {
          Ent_ocaml.loaded_group_edge = "user";
          loaded_group_alias = Some "editor";
          loaded_group_name = "editor";
          loaded_group_rows =
            [
              {
                Ent_ocaml.loaded_edge = "user";
                loaded_alias = Some "editor";
                loaded_name = "editor";
                loaded_source = "Post";
                loaded_target = Some "User";
              };
            ];
        };
      ] ->
      ()
  | Ok _ -> Alcotest.fail "unexpected named load_edges result"
  | Error error -> Alcotest.fail (Ent_ocaml.error_to_string error));
  let module Client = Post.Client (Memory_backend) in
  let client = Client.make () in
  (match
    Client.load_edge_named client ~decode_source:decode ~decode_target:decode
      edge_query
  with
  | Ok
      [
        {
          Ent_ocaml.loaded_edge = "user";
          loaded_alias = Some "author";
          loaded_name = "author";
          loaded_source = "Post";
          loaded_target = Some "User";
        };
      ] ->
      ()
  | Ok _ -> Alcotest.fail "unexpected client named load_edge result"
  | Error error -> Alcotest.fail (Ent_ocaml.error_to_string error));
  match
    Client.load_edges_named client ~decode_source:decode ~decode_target:decode
      [ edge_query; second_edge_query ]
  with
  | Ok [ author_group; editor_group ] ->
      Alcotest.(check string)
        "client first edge group" "author" author_group.loaded_group_name;
      Alcotest.(check string)
        "client second edge group" "editor" editor_group.loaded_group_name
  | Ok _ -> Alcotest.fail "unexpected client named load_edges result"
  | Error error -> Alcotest.fail (Ent_ocaml.error_to_string error)

let test_generated_mutation_api () =
  let open Post in
  let create_mutation =
    create ()
    |> set (id "post_1")
    |> set (user_id "user_1")
    |> set (body "hello")
    |> set (media_ids [ "media_1"; "media_2" ])
    |> set (status "draft")
    |> set (created_at_ms 1_700_000_000L)
    |> set (updated_at_ms 1_700_000_000L)
    |> set (published_at_ms None)
  in
  Alcotest.(check bool)
    "create op" true
    (match create_mutation.op with Ent_ocaml.Create -> true | _ -> false);
  Alcotest.(check int) "create fields" 8 (List.length create_mutation.set);
  let record_1 =
    {
      id = "post_1";
      user_id = "user_1";
      body = "hello";
      media_ids = [];
      status = "draft";
      created_at_ms = 1L;
      updated_at_ms = 1L;
      published_at_ms = None;
    }
  in
  let record_2 =
    {
      record_1 with
      id = "post_2";
      body = "second";
      created_at_ms = 2L;
      updated_at_ms = 2L;
    }
  in
  let create_many =
    create_many [ record_1; record_2 ]
  in
  Alcotest.(check int) "bulk create rows" 2 (List.length create_many);
  Alcotest.(check bool)
    "bulk create ops" true
    (List.for_all
       (fun mutation ->
         match mutation.Ent_ocaml.op with Ent_ocaml.Create -> true | _ -> false)
       create_many);
  let create_with_default =
    create ()
    |> set (id "post_3")
    |> set (user_id "user_1")
    |> set (body "default status")
    |> set (media_ids [])
    |> set (created_at_ms 3L)
    |> set (updated_at_ms 3L)
    |> set (published_at_ms None)
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
    create ()
    |> set (id "post_invalid")
    |> set (user_id "user_1")
    |> set (body "")
    |> set (media_ids [])
    |> set (created_at_ms 4L)
    |> set (updated_at_ms 4L)
    |> set (published_at_ms None)
  in
  (match Ent_ocaml.validate_mutation create_invalid with
  | Ok () -> Alcotest.fail "expected generated validator error"
  | Error (`Bad_query message) ->
      Alcotest.(check string)
        "validator message"
        "validation failed for field body: must not be empty"
        message
  | Error error -> Alcotest.fail (Ent_ocaml.error_to_string error));
  let create_invalid_list =
    create ()
    |> set (id "post_invalid_list")
    |> set (user_id "user_1")
    |> set (body "has too much media")
    |> set (media_ids [ "m1"; "m2"; "m3" ])
    |> set (created_at_ms 4L)
    |> set (updated_at_ms 4L)
    |> set (published_at_ms None)
  in
  (match Ent_ocaml.validate_mutation create_invalid_list with
  | Ok () -> Alcotest.fail "expected generated list validator error"
  | Error (`Bad_query message) ->
      Alcotest.(check string)
        "list validator message"
        "validation failed for field media_ids: too many media items"
        message
  | Error error -> Alcotest.fail (Ent_ocaml.error_to_string error));
  let create_invalid_option =
    create ()
    |> set (id "post_invalid_option")
    |> set (user_id "user_1")
    |> set (body "bad published time")
    |> set (media_ids [])
    |> set (created_at_ms 4L)
    |> set (updated_at_ms 4L)
    |> set (published_at_ms (Some (-1L)))
  in
  (match Ent_ocaml.validate_mutation create_invalid_option with
  | Ok () -> Alcotest.fail "expected generated option validator error"
  | Error (`Bad_query message) ->
      Alcotest.(check string)
        "option validator message"
        "validation failed for field published_at_ms: must not be negative"
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
  let update_from_query =
    let open Post in
    query ()
    |> where (id_eq "post_1")
    |> where (user (Ent_ocaml.Eq ("id", Ent_ocaml.V_string "user_1")))
    |> update_one_where
    |> set (body "from query")
  in
  Alcotest.(check int)
    "update from query predicates" 2
    (List.length update_from_query.predicates);
  let upsert =
    let open Post in
    query ()
    |> where (id_eq "post_1")
    |> upsert_where
    |> set (body "upserted")
    |> on_insert (id "post_1")
    |> on_insert (user_id "user_1")
    |> on_insert (media_ids [])
    |> on_insert (status "draft")
    |> on_insert (created_at_ms 1L)
    |> on_insert (updated_at_ms 1L)
    |> on_insert (published_at_ms None)
  in
  Alcotest.(check bool)
    "upsert one op" true
    (match upsert.op with Ent_ocaml.Upsert_one -> true | _ -> false);
  Alcotest.(check int) "upsert set" 1 (List.length upsert.set);
  Alcotest.(check int) "upsert on insert" 7 (List.length upsert.on_insert);
  (match Ent_ocaml.validate_mutation upsert with
  | Ok () -> ()
  | Error error -> Alcotest.fail (Ent_ocaml.error_to_string error));
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
  let delete_from_query =
    let open Post in
    query ()
    |> where (id_eq "post_1")
    |> delete_one_where
  in
  Alcotest.(check int)
    "delete from query predicates" 1
    (List.length delete_from_query.predicates);
  Alcotest.(check bool)
    "delete one op" true
    (match delete.op with Ent_ocaml.Delete_one -> true | _ -> false)

let test_generated_result_default_api () =
  let create =
    let open Task in
    create_values_result [ id "task_1"; title "write tests"; updated_at_ms 1L ]
  in
  let create =
    match create with
    | Ok mutation -> mutation
    | Error error -> Alcotest.fail (Ent_ocaml.error_to_string error)
  in
  Alcotest.(check bool)
    "create result default" true
    (List.exists
       (function
         | "created_at_ms", Ent_ocaml.V_int64 100L -> true
         | _ -> false)
       create.set);
  let update =
    let open Task in
    update_id_result "task_1"
  in
  let update =
    match update with
    | Ok mutation -> mutation
    | Error error -> Alcotest.fail (Ent_ocaml.error_to_string error)
  in
  Alcotest.(check bool)
    "update id result default" true
    (List.exists
       (function
         | "updated_at_ms", Ent_ocaml.V_int64 200L -> true
         | _ -> false)
       update.set);
  let update_from_query =
    let open Task in
    by_id "task_1"
    |> update_one_where_result
  in
  let update_from_query =
    match update_from_query with
    | Ok mutation -> mutation
    | Error error -> Alcotest.fail (Ent_ocaml.error_to_string error)
  in
  Alcotest.(check int)
    "update where result predicate" 1
    (List.length update_from_query.predicates);
  (match
     FailingTask.create_values_result
       [ FailingTask.id "task_2"; FailingTask.title "fails" ]
   with
  | Error (`Bad_query "clock unavailable") -> ()
  | Ok _ -> Alcotest.fail "expected default_result error"
  | Error error -> Alcotest.fail (Ent_ocaml.error_to_string error))

let test_generated_store_api () =
  let module Store = Post.Store (Memory_backend) in
  let decode = function
    | Ent_ocaml.V_string value -> Ok value
    | _ -> Error "expected string"
  in
  let draft_query =
    Post.query ()
    |> Post.where (Post.user_id_eq "user_1")
    |> Post.where (Post.status_eq "draft")
  in
  (match Store.all () ~decode draft_query with
  | Ok [ "Post" ] -> ()
  | Ok _ -> Alcotest.fail "unexpected all result"
  | Error error -> Alcotest.fail (Ent_ocaml.error_to_string error));
  (match Store.count () draft_query with
  | Ok count -> Alcotest.(check int) "count" 2 count
  | Error error -> Alcotest.fail (Ent_ocaml.error_to_string error));
  let selected_query =
    let open Post in
    query () |> select [ select_id; select_body ]
  in
  (match Store.values () selected_query with
  | Ok [ Ent_ocaml.V_doc fields ] ->
      Alcotest.(check bool) "selected id" true (List.mem_assoc "id" fields);
      Alcotest.(check bool) "selected body" true
        (List.mem_assoc "body" fields)
  | Ok _ -> Alcotest.fail "unexpected selected values result"
  | Error error -> Alcotest.fail (Ent_ocaml.error_to_string error));
  let selected_value_query =
    let open Post in
    query () |> select [ select_body ]
  in
  (match Store.value () selected_value_query with
  | Ok (Some (Ent_ocaml.V_string "body")) -> ()
  | Ok _ -> Alcotest.fail "unexpected selected value result"
  | Error error -> Alcotest.fail (Ent_ocaml.error_to_string error));
  let aggregate =
    let open Post in
    query ()
    |> where (status_eq "draft")
    |> avg select_updated_at_ms
  in
  (match Store.aggregate () aggregate with
  | Ok (Some (Ent_ocaml.V_int64 value)) ->
      Alcotest.(check int64) "aggregate value" 42L value
  | Ok _ -> Alcotest.fail "unexpected aggregate result"
  | Error error -> Alcotest.fail (Ent_ocaml.error_to_string error));
  let scanned =
    let open Post in
    query ()
    |> where (status_eq "draft")
    |> scan [ count_as "posts"; sum_as "created" select_created_at_ms ]
  in
  (match Store.aggregate_scan () scanned with
  | Ok
      [
        ("posts", Some (Ent_ocaml.V_int 2));
        ("created", Some (Ent_ocaml.V_int64 value));
      ] ->
      Alcotest.(check int64) "scan aggregate value" 42L value
  | Ok _ -> Alcotest.fail "unexpected aggregate scan result"
  | Error error -> Alcotest.fail (Ent_ocaml.error_to_string error));
  let grouped =
    let open Post in
    query ()
    |> where (status_eq "draft")
    |> sum select_created_at_ms
    |> group_by select_status
  in
  (match Store.group () grouped with
  | Ok
      [
        {
          Ent_ocaml.group = Ent_ocaml.V_string "status";
          value = Some (Ent_ocaml.V_int64 value);
        };
      ] ->
      Alcotest.(check int64) "group aggregate value" 42L value
  | Ok _ -> Alcotest.fail "unexpected group aggregate result"
  | Error error -> Alcotest.fail (Ent_ocaml.error_to_string error));
  let mutation =
    let open Post in
    create ()
    |> set (id "post_1")
    |> set (user_id "user_1")
    |> set (body "body")
    |> set (media_ids [])
    |> set (created_at_ms 1L)
    |> set (updated_at_ms 1L)
    |> set (published_at_ms None)
  in
  (match Store.insert () mutation with
  | Ok (Ent_ocaml.V_doc fields) ->
      Alcotest.(check bool) "inserted id" true (List.mem_assoc "id" fields)
  | Ok _ -> Alcotest.fail "unexpected insert result"
  | Error error -> Alcotest.fail (Ent_ocaml.error_to_string error));
  let mutation =
    let open Post in
    update_one_where draft_query |> set (body "x")
  in
  (match Store.update_one () mutation with
  | Ok () -> ()
  | Error error -> Alcotest.fail (Ent_ocaml.error_to_string error));
  let upsert =
    let open Post in
    upsert_one ~where:[ id_eq "post_1" ] ()
    |> set (body "upserted")
    |> on_insert (id "post_1")
    |> on_insert (user_id "user_1")
    |> on_insert (media_ids [])
    |> on_insert (status "draft")
    |> on_insert (created_at_ms 1L)
    |> on_insert (updated_at_ms 1L)
    |> on_insert (published_at_ms None)
  in
  match Store.upsert_one () upsert with
  | Ok () -> ()
  | Error error -> Alcotest.fail (Ent_ocaml.error_to_string error)

let test_generated_client_api () =
  let module Client = Post.Client (Memory_backend) in
  let client = Client.make () in
  let decode = function
    | Ent_ocaml.V_string value -> Ok value
    | _ -> Error "expected string"
  in
  let query =
    let open Post in
    query () |> where (status_eq "draft")
  in
  (match Client.all client ~decode query with
  | Ok [ "Post" ] -> ()
  | Ok _ -> Alcotest.fail "unexpected client all result"
  | Error error -> Alcotest.fail (Ent_ocaml.error_to_string error));
  let selected_query =
    let open Post in
    query () |> select [ select_id ]
  in
  (match Client.value client selected_query with
  | Ok (Some (Ent_ocaml.V_string "id")) -> ()
  | Ok _ -> Alcotest.fail "unexpected client selected value result"
  | Error error -> Alcotest.fail (Ent_ocaml.error_to_string error));
  let mutation =
    let open Post in
    create ()
    |> set (id "post_1")
    |> set (user_id "user_1")
    |> set (body "body")
    |> set (media_ids [])
    |> set (created_at_ms 1L)
    |> set (updated_at_ms 1L)
    |> set (published_at_ms None)
  in
  match Client.with_transaction client (fun tx -> Client.Tx.insert tx mutation) with
  | Ok (Ent_ocaml.V_doc fields) ->
      Alcotest.(check bool) "tx inserted id" true (List.mem_assoc "id" fields)
  | Ok _ -> Alcotest.fail "unexpected client tx insert result"
  | Error error -> Alcotest.fail (Ent_ocaml.error_to_string error)

let test_generated_client_transaction_hooks () =
  let module Client = Post.Client (Memory_backend) in
  let client = Client.make () in
  let events = ref [] in
  let hooks =
    [
      Ent_ocaml.Transaction.hook
        ~after_commit:(fun () ->
          events := "commit" :: !events;
          Ok ())
        ~after_rollback:(fun () error ->
          events := Ent_ocaml.error_to_string error :: "rollback" :: !events;
          Ok ())
        ();
    ]
  in
  (match Client.with_transaction ~hooks client (fun _tx -> Ok ()) with
  | Ok () ->
      Alcotest.(check (list string)) "client commit hook" [ "commit" ] !events
  | Error error -> Alcotest.fail (Ent_ocaml.error_to_string error));
  let options = Ent_ocaml.Transaction.options ~max_commit_time_ms:25 () in
  (match Client.with_transaction ~options client (fun _tx -> Ok ()) with
  | Ok () -> ()
  | Error error -> Alcotest.fail (Ent_ocaml.error_to_string error));
  events := [];
  match
    Client.with_transaction ~hooks client (fun _tx ->
        Error (`Bad_query "client rollback"))
  with
  | Error (`Bad_query "client rollback") ->
      Alcotest.(check (list string))
        "client rollback hook"
        [ "bad query: client rollback"; "rollback" ] !events
  | Ok () -> Alcotest.fail "expected client rollback error"
  | Error error -> Alcotest.fail (Ent_ocaml.error_to_string error)

let test_generated_client_hook_api () =
  let module Client = Post.Client (Memory_backend) in
  let module Hooked =
    Client.With_hooks (struct
      let mutation_hooks =
        [
          {
            Ent_ocaml.wrap_mutation =
              (fun next ctx mutation ->
                next ctx
                  (Ent_ocaml.Mutation.set
                     ("body", Ent_ocaml.V_string "client hook")
                     mutation));
          };
        ]
    end)
  in
  let client = Hooked.make () in
  let mutation =
    let open Post in
    create ()
    |> set (id "post_1")
    |> set (user_id "user_1")
    |> set (body "body")
    |> set (media_ids [])
    |> set (created_at_ms 1L)
    |> set (updated_at_ms 1L)
    |> set (published_at_ms None)
  in
  let check_hooked label = function
    | Ok (Ent_ocaml.V_doc fields) ->
        Alcotest.(check (option string))
          label (Some "client hook")
          (match List.assoc_opt "body" fields with
          | Some (Ent_ocaml.V_string value) -> Some value
          | Some _ | None -> None)
    | Ok _ -> Alcotest.fail "unexpected hooked client insert result"
    | Error error -> Alcotest.fail (Ent_ocaml.error_to_string error)
  in
  check_hooked "client hook insert" (Hooked.insert client mutation);
  check_hooked "client hook tx insert"
    (Hooked.with_transaction client (fun tx -> Hooked.Tx.insert tx mutation))

let test_generated_client_policy_api () =
  let module Client = Post.Client (Memory_backend) in
  let module Deny_reads = struct
    let query_rules = [ (fun () _ -> Ent_ocaml.Deny "client no reads") ]
    let mutation_rules = []
  end in
  let module Deny_writes = struct
    let query_rules = []
    let mutation_rules = [ (fun () _ -> Ent_ocaml.Deny "client no writes") ]
  end in
  let module Read_client = Client.With_policy (Deny_reads) in
  let module Write_client = Client.With_policy (Deny_writes) in
  let decode = function
    | Ent_ocaml.V_string value -> Ok value
    | _ -> Error "expected string"
  in
  let query =
    let open Post in
    query () |> where (status_eq "draft")
  in
  let mutation =
    let open Post in
    create ()
    |> set (id "post_1")
    |> set (user_id "user_1")
    |> set (body "body")
    |> set (media_ids [])
    |> set (created_at_ms 1L)
    |> set (updated_at_ms 1L)
    |> set (published_at_ms None)
  in
  let read_client = Read_client.make () in
  let write_client = Write_client.make () in
  (match Read_client.all read_client ~decode query with
  | Error (`Denied "client no reads") -> ()
  | Ok _ -> Alcotest.fail "expected client read denial"
  | Error error -> Alcotest.fail (Ent_ocaml.error_to_string error));
  (match Write_client.insert write_client mutation with
  | Error (`Denied "client no writes") -> ()
  | Ok _ -> Alcotest.fail "expected client write denial"
  | Error error -> Alcotest.fail (Ent_ocaml.error_to_string error));
  match
    Write_client.with_transaction write_client (fun tx ->
        Write_client.Tx.insert tx mutation)
  with
  | Error (`Denied "client no writes") -> ()
  | Ok _ -> Alcotest.fail "expected client tx write denial"
  | Error error -> Alcotest.fail (Ent_ocaml.error_to_string error)

let test_generated_client_interceptor_api () =
  let module Client = Post.Client (Memory_backend) in
  let module Intercepted =
    Client.With_interceptors (struct
      let query_interceptors =
        [
          {
            Ent_ocaml.wrap_query =
              (fun next ctx intercepted_query ->
                let query =
                  let open Post in
                  intercepted_query |> where (status_eq "client intercepted")
                in
                next ctx query);
          };
        ]
    end)
  in
  let module Edge_intercepted =
    Client.With_edge_interceptors (struct
      let edge_interceptors =
        [
          {
            Ent_ocaml.wrap_edge =
              (fun next ctx edge_query ->
                next ctx
                  {
                    edge_query with
                    Ent_ocaml.target = edge_query.Ent_ocaml.source.entity;
                  });
          };
        ]
    end)
  in
  let client = Intercepted.make () in
  let edge_client = Edge_intercepted.make () in
  let query = Post.query () in
  let edge_query =
    let open Post in
    query () |> with_user ~target:user_entity
  in
  let decode = function
    | Ent_ocaml.V_string value -> Ok value
    | _ -> Error "expected string"
  in
  (match Intercepted.count client query with
  | Ok 1 -> ()
  | Ok count -> Alcotest.failf "expected intercepted client count, got %d" count
  | Error error -> Alcotest.fail (Ent_ocaml.error_to_string error));
  (match
    Intercepted.with_transaction client (fun tx -> Intercepted.Tx.count tx query)
  with
  | Ok 1 -> ()
  | Ok count -> Alcotest.failf "expected intercepted tx count, got %d" count
  | Error error -> Alcotest.fail (Ent_ocaml.error_to_string error));
  (match Edge_intercepted.traverse edge_client ~decode edge_query with
  | Ok [ "Post" ] -> ()
  | Ok _ -> Alcotest.fail "unexpected edge-intercepted client traverse result"
  | Error error -> Alcotest.fail (Ent_ocaml.error_to_string error));
  match
    Edge_intercepted.with_transaction edge_client (fun tx ->
        Edge_intercepted.Tx.traverse tx ~decode edge_query)
  with
  | Ok [ "Post" ] -> ()
  | Ok _ -> Alcotest.fail "unexpected edge-intercepted tx traverse result"
  | Error error -> Alcotest.fail (Ent_ocaml.error_to_string error)

let test_generated_schema_client_api () =
  let module Client = Post.Client (Memory_backend) in
  let module Schema_policy = Client.Schema_policy in
  let module Schema_hooks = Client.Schema_hooks in
  let module Schema_interceptors = Client.Schema_interceptors in
  let module Schema_edge_interceptors = Client.Schema_edge_interceptors in
  let decode = function
    | Ent_ocaml.V_string value -> Ok value
    | _ -> Error "expected string"
  in
  let query = Post.query () in
  let mutation =
    let open Post in
    create ()
    |> set (id "post_1")
    |> set (user_id "user_1")
    |> set (body "body")
    |> set (media_ids [])
    |> set (created_at_ms 1L)
    |> set (updated_at_ms 1L)
    |> set (published_at_ms None)
  in
  let policy_client = Schema_policy.make () in
  let hooks_client = Schema_hooks.make () in
  let interceptor_client = Schema_interceptors.make () in
  let edge_interceptor_client = Schema_edge_interceptors.make () in
  (match Schema_policy.all policy_client ~decode query with
  | Error (`Denied "schema no reads") -> ()
  | Ok _ -> Alcotest.fail "expected schema client read denial"
  | Error error -> Alcotest.fail (Ent_ocaml.error_to_string error));
  (match Schema_policy.insert policy_client mutation with
  | Error (`Denied "schema no writes") -> ()
  | Ok _ -> Alcotest.fail "expected schema client write denial"
  | Error error -> Alcotest.fail (Ent_ocaml.error_to_string error));
  (match Schema_hooks.insert hooks_client mutation with
  | Ok (Ent_ocaml.V_doc fields) -> (
      match List.assoc_opt "body" fields with
      | Some (Ent_ocaml.V_string "schema hook") -> ()
      | _ -> Alcotest.fail "expected schema client hook body")
  | Ok _ -> Alcotest.fail "unexpected schema client hook insert result"
  | Error error -> Alcotest.fail (Ent_ocaml.error_to_string error));
  (match Schema_interceptors.count interceptor_client query with
  | Ok 1 -> ()
  | Ok count ->
      Alcotest.failf "expected schema client intercepted count, got %d" count
  | Error error -> Alcotest.fail (Ent_ocaml.error_to_string error));
  let edge_query =
    let open Post in
    query () |> with_user ~target:user_entity
  in
  match
    Schema_edge_interceptors.traverse edge_interceptor_client ~decode edge_query
  with
  | Ok [ "Post" ] -> ()
  | Ok _ -> Alcotest.fail "unexpected schema edge-intercepted client traverse"
  | Error error -> Alcotest.fail (Ent_ocaml.error_to_string error)

let test_generated_policy_store_api () =
  let module Store = Post.Store (Memory_backend) in
  let module Deny_reads = struct
    let query_rules = [ (fun () _ -> Ent_ocaml.Deny "no reads") ]
    let mutation_rules = []
  end in
  let module Deny_writes = struct
    let query_rules = []
    let mutation_rules = [ (fun () _ -> Ent_ocaml.Deny "no writes") ]
  end in
  let module Read_store = Store.With_policy (Deny_reads) in
  let module Write_store = Store.With_policy (Deny_writes) in
  let module Schema_policy = Store.Schema_policy in
  let decode = function
    | Ent_ocaml.V_string value -> Ok value
    | _ -> Error "expected string"
  in
  let query =
    let open Post in
    query () |> where (status_eq "draft")
  in
  (match Read_store.all () ~decode query with
  | Error (`Denied "no reads") -> ()
  | Ok _ -> Alcotest.fail "expected read denial"
  | Error error -> Alcotest.fail (Ent_ocaml.error_to_string error));
  let mutation =
    let open Post in
    create ()
    |> set (id "post_1")
    |> set (user_id "user_1")
    |> set (body "body")
    |> set (media_ids [])
    |> set (created_at_ms 1L)
    |> set (updated_at_ms 1L)
    |> set (published_at_ms None)
  in
  (match Write_store.insert () mutation with
  | Error (`Denied "no writes") -> ()
  | Ok _ -> Alcotest.fail "expected write denial"
  | Error error -> Alcotest.fail (Ent_ocaml.error_to_string error));
  (match Schema_policy.all () ~decode query with
  | Error (`Denied "schema no reads") -> ()
  | Ok _ -> Alcotest.fail "expected schema read denial"
  | Error error -> Alcotest.fail (Ent_ocaml.error_to_string error));
  match Schema_policy.insert () mutation with
  | Error (`Denied "schema no writes") -> ()
  | Ok _ -> Alcotest.fail "expected schema write denial"
  | Error error -> Alcotest.fail (Ent_ocaml.error_to_string error)

let test_generated_hook_store_api () =
  let module Store = Post.Store (Memory_backend) in
  let module Schema_hooks = Store.Schema_hooks in
  let module Hooked = Store.With_hooks (struct
    let mutation_hooks =
      [
        {
          Ent_ocaml.wrap_mutation =
            (fun next ctx mutation ->
              let mutation =
                let open Post in
                mutation |> set (status "hooked")
              in
              next ctx mutation);
        };
      ]
  end) in
  let mutation =
    let open Post in
    create ()
    |> set (id "post_1")
    |> set (user_id "user_1")
    |> set (body "body")
    |> set (media_ids [])
    |> set (created_at_ms 1L)
    |> set (updated_at_ms 1L)
    |> set (published_at_ms None)
  in
  (match Hooked.insert () mutation with
  | Ok (Ent_ocaml.V_doc fields) -> (
      match List.assoc_opt "status" fields with
      | Some (Ent_ocaml.V_string "hooked") -> ()
      | _ -> Alcotest.fail "expected hooked status")
  | Ok _ -> Alcotest.fail "unexpected insert result"
  | Error error -> Alcotest.fail (Ent_ocaml.error_to_string error));
  (match Schema_hooks.insert () mutation with
  | Ok (Ent_ocaml.V_doc fields) -> (
      match List.assoc_opt "body" fields with
      | Some (Ent_ocaml.V_string "schema hook") -> ()
      | _ -> Alcotest.fail "expected schema hook body")
  | Ok _ -> Alcotest.fail "unexpected schema hook insert result"
  | Error error -> Alcotest.fail (Ent_ocaml.error_to_string error));
  match Hooked.insert_many () [ mutation ] with
  | Ok [ Ent_ocaml.V_doc fields ] -> (
      match List.assoc_opt "status" fields with
      | Some (Ent_ocaml.V_string "hooked") -> ()
      | _ -> Alcotest.fail "expected hooked bulk status")
  | Ok _ -> Alcotest.fail "unexpected bulk insert result"
  | Error error -> Alcotest.fail (Ent_ocaml.error_to_string error)

let test_generated_interceptor_store_api () =
  let module Store = Post.Store (Memory_backend) in
  let module Schema_interceptors = Store.Schema_interceptors in
  let module Schema_edge_interceptors = Store.Schema_edge_interceptors in
  let module Intercepted = Store.With_interceptors (struct
    let query_interceptors =
      [
        {
          Ent_ocaml.wrap_query =
            (fun next ctx intercepted_query ->
              let query =
                let open Post in
                intercepted_query |> where (status_eq "intercepted")
              in
              next ctx query);
        };
      ]
  end) in
  let module Edge_intercepted = Store.With_edge_interceptors (struct
    let edge_interceptors =
      [
        {
          Ent_ocaml.wrap_edge =
            (fun next ctx edge_query ->
              next ctx
                {
                  edge_query with
                  Ent_ocaml.target = edge_query.Ent_ocaml.source.entity;
                });
        };
      ]
  end) in
  let decode = function
    | Ent_ocaml.V_string value -> Ok value
    | _ -> Error "expected string"
  in
  let query = Post.query () in
  (match Intercepted.all () ~decode query with
  | Ok [ "Post" ] -> ()
  | Ok _ -> Alcotest.fail "unexpected all result"
  | Error error -> Alcotest.fail (Ent_ocaml.error_to_string error));
  (match Intercepted.count () query with
  | Ok 1 -> ()
  | Ok count -> Alcotest.failf "expected intercepted count, got %d" count
  | Error error -> Alcotest.fail (Ent_ocaml.error_to_string error));
  (match Schema_interceptors.count () query with
  | Ok 1 -> ()
  | Ok count -> Alcotest.failf "expected schema intercepted count, got %d" count
  | Error error -> Alcotest.fail (Ent_ocaml.error_to_string error));
  let aggregate =
    let open Post in
    query () |> avg select_updated_at_ms
  in
  (match Intercepted.aggregate () aggregate with
  | Ok (Some (Ent_ocaml.V_int64 42L)) -> ()
  | Ok _ -> Alcotest.fail "unexpected aggregate result"
  | Error error -> Alcotest.fail (Ent_ocaml.error_to_string error));
  let edge_query =
    let open Post in
    query () |> with_user ~target:user_entity
  in
  (match Edge_intercepted.traverse () ~decode edge_query with
  | Ok [ "Post" ] -> ()
  | Ok _ -> Alcotest.fail "unexpected edge-intercepted traverse result"
  | Error error -> Alcotest.fail (Ent_ocaml.error_to_string error));
  match Schema_edge_interceptors.load_edge () ~decode_source:decode
          ~decode_target:decode edge_query with
  | Ok [ ("Post", Some "Post") ] -> ()
  | Ok _ -> Alcotest.fail "unexpected schema edge-intercepted load result"
  | Error error -> Alcotest.fail (Ent_ocaml.error_to_string error)

let test_generated_dynamic_filter_api () =
  let open Ent_ocaml.Result_syntax in
  let result =
    let open Post in
    let filter =
      dynamic_filter ~field:select_status
        ~value:(Ent_ocaml.V_string "draft")
        Ent_ocaml.Dynamic_filter.Equal
    in
    let* predicate = dynamic_predicate filter in
    let* query = query () |> where_dynamic filter in
    Ok (predicate, query)
  in
  match result with
  | Ok (Ent_ocaml.Eq ("status", V_string "draft"), query) ->
      Alcotest.(check int) "dynamic predicates" 1
        (List.length query.predicates)
  | Ok _ -> Alcotest.fail "unexpected generated dynamic filter result"
  | Error error -> Alcotest.fail (Ent_ocaml.error_to_string error)

let test_generated_entql_api () =
  let open Ent_ocaml.Result_syntax in
  let result =
    let open Post in
    let* predicate = entql_predicate {|status == "draft"|} in
    let* query =
      query ()
      |> where_entql
           {|status == "draft" || (body contains "hello" && !published_at_ms is_null)|}
    in
    Ok (predicate, query)
  in
  match result with
  | ( Ok
        ( Ent_ocaml.Eq ("status", Ent_ocaml.V_string "draft"),
          {
            Ent_ocaml.predicates =
              [
                Ent_ocaml.Or
                  [
                    Ent_ocaml.Eq ("status", Ent_ocaml.V_string "draft");
                    Ent_ocaml.And
                      [
                        Ent_ocaml.Contains ("body", "hello");
                        Ent_ocaml.Not (Ent_ocaml.Is_nil "published_at_ms");
                      ];
                  ];
              ];
            _;
          } ) ) ->
      ()
  | Ok _ -> Alcotest.fail "unexpected generated entql result"
  | Error error -> Alcotest.fail (Ent_ocaml.error_to_string error)

let test_generated_entql_edge_path_api () =
  let open Ent_ocaml.Result_syntax in
  let result =
    let open Post in
    query () |> where_entql {|user.id == "user_1"|}
  in
  match result with
  | Ok
      {
        Ent_ocaml.predicates =
          [
            Ent_ocaml.Has_edge_with
              ("user", [ Ent_ocaml.Eq ("id", Ent_ocaml.V_string "user_1") ]);
          ];
        _;
      } ->
      ()
  | Ok _ -> Alcotest.fail "unexpected generated entql edge path result"
  | Error error -> Alcotest.fail (Ent_ocaml.error_to_string error)

let test_generated_json_predicate_api () =
  let query =
    let open Event in
    query ()
    |> where
         (metadata_path_eq [ "flags"; "pinned" ] (Ent_ocaml.V_bool true))
    |> where (metadata_path_not_nil [ "author"; "id" ])
    |> order_by [ metadata_path_order ~direction:Ent_ocaml.Desc [ "priority" ] ]
  in
  Alcotest.(check int) "json predicates" 2 (List.length query.predicates);
  Alcotest.(check (list string))
    "json order" [ "metadata.priority" ]
    (List.map Ent_ocaml.Order.field_name query.orders);
  let record =
    {
      id = "event_1";
      metadata =
        Ent_ocaml.(
          V_doc
            [
              ("flags", V_doc [ ("pinned", V_bool true) ]);
              ("author", V_doc [ ("id", V_string "user_1") ]);
            ]);
    }
  in
  let mutation = Event.create_record record in
  Alcotest.(check bool)
    "metadata field" true
    (match List.assoc_opt "metadata" mutation.set with
    | Some (Ent_ocaml.V_doc _) -> true
    | _ -> false)

let test_generated_nested_value_api () =
  let state_value = { kind = "confirmed"; external_id = Some "ext_1" } in
  let value = publish_state_to_ent_value state_value in
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
    let open PublishAttempt in
    create ()
    |> set (id "attempt_1")
    |> set (state state_value)
  in
  Alcotest.(check bool)
    "nested create" true
    (match create.set with
    | [ ("id", Ent_ocaml.V_string "attempt_1"); ("state", V_doc _) ] -> true
    | _ -> false);
  (match Ent_ocaml.validate_mutation create with
  | Ok () -> ()
  | Error error -> Alcotest.fail (Ent_ocaml.error_to_string error));
  let invalid =
    let open PublishAttempt in
    create ()
    |> set (id "attempt_2")
    |> set (state { kind = ""; external_id = None })
  in
  (match Ent_ocaml.validate_mutation invalid with
  | Ok () -> Alcotest.fail "expected nested validator error"
  | Error (`Bad_query message) ->
      Alcotest.(check string)
        "nested validator message"
        "validation failed for field state: kind must not be empty"
        message
  | Error error -> Alcotest.fail (Ent_ocaml.error_to_string error))

let () =
  Alcotest.run "ent-ocaml-ppx"
    [
      ( "deriving",
        [
          Alcotest.test_case "entity metadata" `Quick test_entity_metadata;
          Alcotest.test_case "generated schema snapshot" `Quick
            test_generated_schema_snapshot;
          Alcotest.test_case "generated schema manifest" `Quick
            test_generated_schema_manifest;
          Alcotest.test_case "generated query api" `Quick
            test_generated_query_api;
          Alcotest.test_case "generated order value api" `Quick
            test_generated_order_value_api;
          Alcotest.test_case "generated edge order api" `Quick
            test_generated_edge_order_api;
          Alcotest.test_case "generated edge count order api" `Quick
            test_generated_edge_count_order_api;
          Alcotest.test_case "generated id api" `Quick
            test_generated_id_api;
          Alcotest.test_case "generated cursor api" `Quick
            test_generated_cursor_api;
          Alcotest.test_case "generated composite cursor api" `Quick
            test_generated_composite_cursor_api;
          Alcotest.test_case "generated traversal api" `Quick
            test_generated_traversal_api;
          Alcotest.test_case "generated mutation api" `Quick
            test_generated_mutation_api;
          Alcotest.test_case "generated result default api" `Quick
            test_generated_result_default_api;
          Alcotest.test_case "generated store api" `Quick
            test_generated_store_api;
          Alcotest.test_case "generated client api" `Quick
            test_generated_client_api;
          Alcotest.test_case "generated client transaction hooks" `Quick
            test_generated_client_transaction_hooks;
          Alcotest.test_case "generated client hook api" `Quick
            test_generated_client_hook_api;
          Alcotest.test_case "generated client policy api" `Quick
            test_generated_client_policy_api;
          Alcotest.test_case "generated client interceptor api" `Quick
            test_generated_client_interceptor_api;
          Alcotest.test_case "generated schema client api" `Quick
            test_generated_schema_client_api;
          Alcotest.test_case "generated policy store api" `Quick
            test_generated_policy_store_api;
          Alcotest.test_case "generated hook store api" `Quick
            test_generated_hook_store_api;
          Alcotest.test_case "generated interceptor store api" `Quick
            test_generated_interceptor_store_api;
          Alcotest.test_case "generated dynamic filter api" `Quick
            test_generated_dynamic_filter_api;
          Alcotest.test_case "generated entql api" `Quick
            test_generated_entql_api;
          Alcotest.test_case "generated entql edge path api" `Quick
            test_generated_entql_edge_path_api;
          Alcotest.test_case "generated json predicate api" `Quick
            test_generated_json_predicate_api;
          Alcotest.test_case "generated nested value api" `Quick
            test_generated_nested_value_api;
        ] );
    ]
