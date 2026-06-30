let env name default =
  match Sys.getenv_opt name with Some "" | None -> default | Some value -> value

let host = env "POSTER_MONGO_HOST" "oracle-vm"
let port = env "POSTER_MONGO_PORT" "27017" |> int_of_string

let db =
  Printf.sprintf "ent_ocaml_bulk_e2e_%d_%d" (Unix.getpid ()) (Random.bits ())

let user_entity =
  Ent_ocaml.
    {
      name = "User";
      collection = "users";
      fields =
        [
          {
            name = "id";
            storage_key = "_id";
            typ = String;
            required = true;
            unique = true;
            immutable = false;
            nillable = false;
            validators = [];
            sensitive = false;
            deprecated = None;
            comment = None;
          };
          {
            name = "username";
            storage_key = "username";
            typ = String;
            required = true;
            unique = true;
            immutable = false;
            nillable = false;
            validators = [];
            sensitive = false;
            deprecated = None;
            comment = None;
          };
        ];
      edges = [];
      indexes =
        [
          {
            name = Some "unique_username";
            fields = [ "username" ];
            edges = [];
            unique = true;
            partial_filter = [];
          };
        ];
    }

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
            immutable = false;
            nillable = false;
            validators = [];
            sensitive = false;
            deprecated = None;
            comment = None;
          };
          {
            name = "body";
            storage_key = "body";
            typ = String;
            required = true;
            unique = false;
            immutable = false;
            nillable = false;
            validators = [];
            sensitive = false;
            deprecated = None;
            comment = None;
          };
          {
            name = "user_id";
            storage_key = "user_id";
            typ = String;
            required = true;
            unique = false;
            immutable = false;
            nillable = false;
            validators = [];
            sensitive = false;
            deprecated = None;
            comment = None;
          };
          {
            name = "views";
            storage_key = "views";
            typ = Int64;
            required = false;
            unique = false;
            immutable = false;
            nillable = false;
            validators = [];
            sensitive = false;
            deprecated = None;
            comment = None;
          };
          {
            name = "metadata";
            storage_key = "metadata";
            typ = Json;
            required = false;
            unique = false;
            immutable = false;
            nillable = false;
            validators = [];
            sensitive = false;
            deprecated = None;
            comment = None;
          };
        ];
      edges =
        [
          {
            name = "user";
            target = "User";
            direction = To;
            cardinality = One;
            required = true;
            storage_key = Some "user_id";
          };
        ];
      indexes =
        [
          {
            name = Some "posts_by_id_body";
            fields = [ "id"; "body" ];
            edges = [];
            unique = false;
            partial_filter = [];
          };
          {
            name = Some "posts_with_metadata_by_body";
            fields = [ "body" ];
            edges = [];
            unique = false;
            partial_filter = [ Eq ("user_id", V_string "user_1") ];
          };
        ];
    }

let create_user id username =
  Ent_ocaml.
    {
      entity = user_entity;
      op = Create;
      predicates = [];
      set = [ ("id", V_string id); ("username", V_string username) ];
      clear = [];
      add = [];
      on_insert = [];
    }

let create id user_id body views =
  let pinned = id = "post_2" in
  let priority = if id = "post_2b" then 30 else if id = "post_2" then 20 else 10 in
  Ent_ocaml.
    {
      entity = post_entity;
      op = Create;
      predicates = [];
      set =
        [
          ("id", V_string id);
          ("user_id", V_string user_id);
          ("body", V_string body);
          ("views", V_int64 views);
          ( "metadata",
            V_doc
              [
                ("flags", V_doc [ ("pinned", V_bool pinned) ]);
                ("priority", V_int priority);
              ] );
        ];
      clear = [];
      add = [];
      on_insert = [];
    }

let invalid_create_missing_body =
  Ent_ocaml.
    {
      entity = post_entity;
      op = Create;
      predicates = [];
      set =
        [ ("id", V_string "post_missing_body"); ("user_id", V_string "user_1") ];
      clear = [];
      add = [];
      on_insert = [];
    }

let upsert id user_id body =
  Ent_ocaml.
    {
      entity = post_entity;
      op = Upsert_one;
      predicates = [ Eq ("id", V_string id) ];
      set = [ ("body", V_string body) ];
      clear = [];
      add = [];
      on_insert =
        [
          ("id", V_string id);
          ("user_id", V_string user_id);
          ("views", V_int64 0L);
        ];
    }

let query_all =
  Ent_ocaml.
    {
      entity = post_entity;
      predicates = [];
      select = [];
      orders = [ { field = "id"; direction = Asc; value_alias = None } ];
      limit = None;
      offset = None;
    }

let query_user_1 =
  Ent_ocaml.
    {
      entity = post_entity;
      predicates = [ Has_edge_with ("user", [ Eq ("id", V_string "user_1") ]) ];
      select = [];
      orders = [ { field = "id"; direction = Asc; value_alias = None } ];
      limit = None;
      offset = None;
    }

let query_after_post_1 =
  Ent_ocaml.Query.make post_entity
  |> Ent_ocaml.Query.after ~field:"id" ~direction:Ent_ocaml.Asc
       (Ent_ocaml.V_string "post_1")
  |> Ent_ocaml.Query.limit 1

let query_after_views_post_2 =
  Ent_ocaml.Query.make post_entity
  |> Ent_ocaml.Query.after_cursor
       Ent_ocaml.
         [
           { field = "views"; direction = Asc; value = V_int64 20L };
           { field = "id"; direction = Asc; value = V_string "post_2" };
         ]
  |> Ent_ocaml.Query.limit 1

let query_user_from_posts =
  Ent_ocaml.Edge_query.make ~edge:"user" ~target:user_entity query_user_1

let assert_true label condition =
  if condition then Printf.printf "PASS %s\n%!" label
  else failwith ("FAIL " ^ label)

let assert_int64_value label expected = function
  | Some (Ent_ocaml.V_int64 value) ->
      assert_true label (value = expected)
  | Some (Ent_ocaml.V_int32 value) ->
      assert_true label (Int64.of_int32 value = expected)
  | Some (Ent_ocaml.V_int value) ->
      assert_true label (Int64.of_int value = expected)
  | _ -> failwith ("FAIL " ^ label)

let assert_float_value label expected = function
  | Some (Ent_ocaml.V_float value) ->
      assert_true label (Float.abs (value -. expected) < 0.000_001)
  | Some (Ent_ocaml.V_int64 value) ->
      assert_true label (Int64.to_float value = expected)
  | Some (Ent_ocaml.V_int32 value) ->
      assert_true label (Int32.to_float value = expected)
  | Some (Ent_ocaml.V_int value) ->
      assert_true label (Float.of_int value = expected)
  | _ -> failwith ("FAIL " ^ label)

let value_to_int64 = function
  | Ent_ocaml.V_int64 value -> Some value
  | Ent_ocaml.V_int32 value -> Some (Int64.of_int32 value)
  | Ent_ocaml.V_int value -> Some (Int64.of_int value)
  | _ -> None

let assoc_int64 key values =
  match List.assoc_opt key values with
  | Some (Some value) -> value_to_int64 value
  | Some None | None -> None

let group_value key groups =
  match
    List.find_opt
      (fun result -> result.Ent_ocaml.group = Ent_ocaml.V_string key)
      groups
  with
  | None -> None
  | Some result -> (
      match result.Ent_ocaml.value with
      | None -> None
      | Some value -> value_to_int64 value)

let cleanup client =
  Mongo_eio.direct_run_command client db [ ("dropDatabase", Bson.create_int32 1l) ]
  |> Result.map (fun _ -> ())

let check_partial_index client =
  match
    Mongo_eio.direct_run_command client db
      [ ("listIndexes", Bson.create_string post_entity.collection) ]
  with
  | Error error -> Error (`Backend (Mongo_error.to_string error))
  | Ok response ->
      let indexes = Mongo_command.cursor_batch response.Mongo_command.body in
      let has_partial =
        List.exists
          (fun index ->
            match
              ( Bson.get_string (Bson.get_element "name" index),
                Bson.get_doc_element
                  (Bson.get_element "partialFilterExpression" index) )
            with
            | "posts_with_metadata_by_body", partial ->
                Bson.get_string (Bson.get_element "user_id" partial) = "user_1"
            | _ -> false
            | exception _ -> false)
          indexes
      in
      assert_true "partial index created" has_partial;
      Ok ()

let check_index_drift ctx =
  let open Ent_ocaml.Result_syntax in
  let* checks = Ent_ocaml_mongo.check_indexes ctx [ user_entity; post_entity ] in
  assert_true "index drift check passes"
    (List.for_all Ent_ocaml_mongo.index_check_ok checks);
  let drift_entity =
    Ent_ocaml.
      {
        post_entity with
        indexes =
          post_entity.indexes
          @ [
              {
                name = Some "missing_index_for_drift_check";
                fields = [ "views" ];
                edges = [];
                unique = false;
                partial_filter = [];
              };
            ];
      }
  in
  let* drift_checks = Ent_ocaml_mongo.check_indexes ctx [ drift_entity ] in
  assert_true "index drift check reports missing"
    (List.exists
       (fun (check : Ent_ocaml_mongo.index_check) ->
         check.name = "missing_index_for_drift_check"
         &&
         match check.status with
         | Ent_ocaml_mongo.Missing -> true
         | Ent_ocaml_mongo.Present | Ent_ocaml_mongo.Mismatched _ -> false)
       drift_checks);
  match Ent_ocaml_mongo.verify_indexes ctx [ drift_entity ] with
  | Ok () -> Error (`Bad_query "expected index drift verification failure")
  | Error (`Bad_schema _) -> Ok ()
  | Error _ as error -> error

let run_flow client =
  let open Ent_ocaml.Result_syntax in
  let ctx = Ent_ocaml_mongo.create ~client { database = db } in
  let* () = Ent_ocaml_mongo.ensure_indexes ctx [ user_entity; post_entity ] in
  let* () = check_index_drift ctx in
  let* () = check_partial_index client in
  let* _users =
    Ent_ocaml_mongo.insert_many_values ctx
      [ create_user "user_1" "alice"; create_user "user_2" "bob" ]
  in
  let* docs =
    Ent_ocaml_mongo.insert_many_values ctx
      [
        create "post_1" "user_1" "first" 10L;
        create "post_2" "user_2" "second" 20L;
        create "post_2b" "user_2" "second-b" 20L;
      ]
  in
  assert_true "bulk insert returns docs" (List.length docs = 3);
  let* found = Ent_ocaml_mongo.find ctx query_all in
  assert_true "bulk insert persisted rows" (List.length found = 3);
  let* selected_rows =
    Ent_ocaml_mongo.values ctx
      Ent_ocaml.{ query_all with select = [ "id"; "body" ]; limit = Some 1 }
  in
  assert_true "selected values return logical fields"
    (match selected_rows with
    | [ Ent_ocaml.V_doc fields ] ->
        fields
        = Ent_ocaml.
            [ ("id", V_string "post_1"); ("body", V_string "first") ]
    | _ -> false);
  let* selected_value =
    Ent_ocaml_mongo.value ctx
      Ent_ocaml.{ query_all with select = [ "body" ]; limit = Some 1 }
  in
  assert_true "selected value returns first field"
    (selected_value = Some (Ent_ocaml.V_string "first"));
  let* selected_order_value =
    Ent_ocaml_mongo.value ctx
      Ent_ocaml.
        {
          query_all with
          orders =
            [
              {
                field = "views";
                direction = Desc;
                value_alias = Some "ordered_views";
              };
            ];
          limit = Some 1;
        }
  in
  assert_true "selected order value returns sort term"
    (selected_order_value = Some (Ent_ocaml.V_int64 20L));
  let* dynamic_user_posts =
    match
      Ent_ocaml.Query.make post_entity
      |> Ent_ocaml.Dynamic_filter.where
           (Ent_ocaml.Dynamic_filter.make ~field:"user_id"
              ~value:(Ent_ocaml.V_string "user_2")
              Ent_ocaml.Dynamic_filter.Equal)
    with
    | Ok query -> Ent_ocaml_mongo.find ctx query
    | Error _ as error -> error
  in
  assert_true "dynamic filter returns user posts"
    (List.length dynamic_user_posts = 2);
  let* pinned_posts =
    Ent_ocaml_mongo.find ctx
      (Ent_ocaml.Query.make post_entity
         ~where:
           Ent_ocaml.
             [ Json_eq ("metadata", [ "flags"; "pinned" ], V_bool true) ])
  in
  assert_true "json path predicate returns pinned post"
    (match pinned_posts with
    | [ doc ] -> Bson.get_string (Bson.get_element "_id" doc) = "post_2"
    | _ -> false);
  let* priority_posts =
    Ent_ocaml_mongo.find ctx
      Ent_ocaml.
        {
          entity = post_entity;
          predicates = [];
          select = [];
          orders = [ { field = "metadata.priority"; direction = Desc; value_alias = None } ];
          limit = Some 2;
          offset = None;
        }
  in
  assert_true "json path order returns high priority first"
    (match priority_posts with
    | first :: second :: _ ->
        Bson.get_string (Bson.get_element "_id" first) = "post_2b"
        && Bson.get_string (Bson.get_element "_id" second) = "post_2"
    | _ -> false);
  let* selected_json_order_value =
    Ent_ocaml_mongo.value ctx
      Ent_ocaml.
        {
          query_all with
          orders =
            [
              {
                field = "metadata.priority";
                direction = Desc;
                value_alias = Some "priority";
              };
            ];
          limit = Some 1;
        }
  in
  assert_true "selected json order value returns sort term"
    (selected_json_order_value = Some (Ent_ocaml.V_int64 30L));
  let* page = Ent_ocaml_mongo.find ctx query_after_post_1 in
  assert_true "seek pagination returns next row"
    (match page with
    | [ doc ] -> Bson.get_string (Bson.get_element "_id" doc) = "post_2"
    | _ -> false);
  let* composite_page = Ent_ocaml_mongo.find ctx query_after_views_post_2 in
  assert_true "composite cursor returns tied next row"
    (match composite_page with
    | [ doc ] -> Bson.get_string (Bson.get_element "_id" doc) = "post_2b"
    | _ -> false);
  let* user_posts = Ent_ocaml_mongo.find ctx query_user_1 in
  assert_true "edge predicate returns user posts" (List.length user_posts = 1);
  let* traversed_users =
    Ent_ocaml_mongo.traverse_as ctx query_user_from_posts ~decode:(fun doc ->
        Ok (Bson.get_string (Bson.get_element "_id" doc)))
  in
  assert_true "stored edge traversal returns user"
    (traversed_users = [ "user_1" ]);
  let* loaded_users =
    Ent_ocaml_mongo.load_edge_as ctx query_user_from_posts
      ~decode_source:(fun doc ->
        Ok (Bson.get_string (Bson.get_element "body" doc)))
      ~decode_target:(fun doc ->
        Ok (Bson.get_string (Bson.get_element "_id" doc)))
  in
  assert_true "stored edge eager load returns source and user"
    (loaded_users = [ ("first", Some "user_1") ]);
  let* sum =
    Ent_ocaml_mongo.aggregate ctx (Ent_ocaml.Aggregate.sum "views" query_user_1)
  in
  assert_int64_value "aggregate sum returns user views" 10L sum;
  let* avg =
    Ent_ocaml_mongo.aggregate ctx (Ent_ocaml.Aggregate.avg "views" query_all)
  in
  assert_float_value "aggregate avg returns all views" (50.0 /. 3.0) avg;
  let* scan =
    Ent_ocaml_mongo.aggregate_scan ctx
      (Ent_ocaml.Aggregate.scan
         Ent_ocaml.Aggregate.[ count_as "posts"; sum_as "views" "views" ]
         query_all)
  in
  assert_true "aggregate scan returns count and sum"
    (assoc_int64 "posts" scan = Some 3L && assoc_int64 "views" scan = Some 50L);
  let* grouped =
    Ent_ocaml_mongo.group ctx
      (Ent_ocaml.Aggregate.group_by "user_id"
         (Ent_ocaml.Aggregate.sum "views" query_all))
  in
  assert_true "group aggregate returns user_1 views"
    (group_value "user_1" grouped = Some 10L);
  assert_true "group aggregate returns user_2 views"
    (group_value "user_2" grouped = Some 40L);
  let* () =
    Ent_ocaml_mongo.upsert_one ctx (upsert "post_3" "user_1" "third")
  in
  let* found_after_insert = Ent_ocaml_mongo.find ctx query_all in
  assert_true "upsert inserts missing row" (List.length found_after_insert = 4);
  let* () =
    Ent_ocaml_mongo.upsert_one ctx
      (upsert "post_3" "user_1" "third updated")
  in
  let* user_posts_after_upsert = Ent_ocaml_mongo.find ctx query_user_1 in
  assert_true "upsert updates existing row"
    (List.exists
       (fun doc ->
         Bson.get_string (Bson.get_element "_id" doc) = "post_3"
         && Bson.get_string (Bson.get_element "body" doc) = "third updated")
       user_posts_after_upsert);
  let duplicate =
    Ent_ocaml_mongo.insert_many_values ctx
      [ create "dupe" "user_1" "duplicate" 1L; create "dupe" "user_1" "duplicate" 1L ]
  in
  (match duplicate with
  | Error (`Constraint _) -> Printf.printf "PASS bulk duplicate maps to constraint\n%!"
  | Ok _ -> failwith "FAIL bulk duplicate maps to constraint"
  | Error error -> failwith ("FAIL unexpected duplicate error: " ^ Ent_ocaml.error_to_string error));
  (match Ent_ocaml_mongo.insert_many_values ctx [ invalid_create_missing_body ] with
  | Error (`Bad_query _) ->
      Printf.printf "PASS bulk create validates required fields\n%!"
  | Ok _ -> failwith "FAIL bulk create validates required fields"
  | Error error ->
      failwith
        ("FAIL unexpected validation error: " ^ Ent_ocaml.error_to_string error));
  Ok ()

let () =
  Random.self_init ();
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let config =
    {
      (Mongo_config.default ~host ~port ~database:db ())
      with
      max_pool_size = 4;
      min_pool_size = 1;
      socket_timeout_ms = Some 10_000;
      wait_queue_timeout_ms = 10_000;
    }
  in
  let result =
    match
      Mongo_eio.connect ~sw ~net:(Eio.Stdenv.net env)
        ~clock:(Eio.Stdenv.clock env) ~config
    with
    | Error error -> Error (`Backend (Mongo_error.to_string error))
    | Ok client ->
        Fun.protect
          ~finally:(fun () ->
            ignore (cleanup client);
            Mongo_eio.close_direct client)
          (fun () -> run_flow client)
  in
  match result with
  | Ok () -> ()
  | Error error -> failwith (Ent_ocaml.error_to_string error)
