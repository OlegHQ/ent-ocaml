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

type event = {
  id : string;
  metadata : Ent_ocaml.value [@ent.json] [@ent.optional];
}
[@@ent.entity "Event"] [@@ent.collection "events"]
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
    Ok
      [
        Ent_ocaml.V_doc
          (List.map
             (fun field -> (field, Ent_ocaml.V_string field))
             query.Ent_ocaml.select);
      ]

  let value () (query : Ent_ocaml.query) =
    match query.Ent_ocaml.select with
    | [ field ] -> Ok (Some (Ent_ocaml.V_string field))
    | [] | _ :: _ :: _ -> Error (`Bad_query "value expects one selected field")

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

  let transaction ctx f = f ctx
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
        | _ -> false)
  | _ -> Alcotest.fail "expected generated schema snapshot document"

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
    | [ { Ent_ocaml.field = "created_at_ms"; direction = Desc } ] -> true
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
    |> with_user ~target:user_entity
  in
  Alcotest.(check string)
    "source entity" "Post" edge_query.Ent_ocaml.source.entity.name;
  Alcotest.(check string) "edge" "user" edge_query.edge;
  Alcotest.(check string) "target entity" "User" edge_query.target.name;
  Alcotest.(check int)
    "source predicates" 1
    (List.length edge_query.source.predicates);
  let module Store = Post.Store (Memory_backend) in
  let decode = function
    | Ent_ocaml.V_string value -> Ok value
    | _ -> Error "expected string"
  in
  (match Store.traverse () ~decode edge_query with
  | Ok [ "User" ] -> ()
  | Ok _ -> Alcotest.fail "unexpected traverse result"
  | Error error -> Alcotest.fail (Ent_ocaml.error_to_string error));
  match
    Store.load_edge () ~decode_source:decode ~decode_target:decode edge_query
  with
  | Ok [ ("Post", Some "User") ] -> ()
  | Ok _ -> Alcotest.fail "unexpected load_edge result"
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
  match Write_store.insert () mutation with
  | Error (`Denied "no writes") -> ()
  | Ok _ -> Alcotest.fail "expected write denial"
  | Error error -> Alcotest.fail (Ent_ocaml.error_to_string error)

let test_generated_hook_store_api () =
  let module Store = Post.Store (Memory_backend) in
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
  match Hooked.insert_many () [ mutation ] with
  | Ok [ Ent_ocaml.V_doc fields ] -> (
      match List.assoc_opt "status" fields with
      | Some (Ent_ocaml.V_string "hooked") -> ()
      | _ -> Alcotest.fail "expected hooked bulk status")
  | Ok _ -> Alcotest.fail "unexpected bulk insert result"
  | Error error -> Alcotest.fail (Ent_ocaml.error_to_string error)

let test_generated_interceptor_store_api () =
  let module Store = Post.Store (Memory_backend) in
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
  let aggregate =
    let open Post in
    query () |> avg select_updated_at_ms
  in
  match Intercepted.aggregate () aggregate with
  | Ok (Some (Ent_ocaml.V_int64 42L)) -> ()
  | Ok _ -> Alcotest.fail "unexpected aggregate result"
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
    (List.map (fun (order : Ent_ocaml.order) -> order.field) query.orders);
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
    | _ -> false)

let () =
  Alcotest.run "ent-ocaml-ppx"
    [
      ( "deriving",
        [
          Alcotest.test_case "entity metadata" `Quick test_entity_metadata;
          Alcotest.test_case "generated schema snapshot" `Quick
            test_generated_schema_snapshot;
          Alcotest.test_case "generated query api" `Quick
            test_generated_query_api;
          Alcotest.test_case "generated cursor api" `Quick
            test_generated_cursor_api;
          Alcotest.test_case "generated composite cursor api" `Quick
            test_generated_composite_cursor_api;
          Alcotest.test_case "generated traversal api" `Quick
            test_generated_traversal_api;
          Alcotest.test_case "generated mutation api" `Quick
            test_generated_mutation_api;
          Alcotest.test_case "generated store api" `Quick
            test_generated_store_api;
          Alcotest.test_case "generated client api" `Quick
            test_generated_client_api;
          Alcotest.test_case "generated policy store api" `Quick
            test_generated_policy_store_api;
          Alcotest.test_case "generated hook store api" `Quick
            test_generated_hook_store_api;
          Alcotest.test_case "generated interceptor store api" `Quick
            test_generated_interceptor_store_api;
          Alcotest.test_case "generated dynamic filter api" `Quick
            test_generated_dynamic_filter_api;
          Alcotest.test_case "generated json predicate api" `Quick
            test_generated_json_predicate_api;
          Alcotest.test_case "generated nested value api" `Quick
            test_generated_nested_value_api;
        ] );
    ]
