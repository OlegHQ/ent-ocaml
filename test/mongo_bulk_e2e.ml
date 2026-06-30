let env name default =
  match Sys.getenv_opt name with Some "" | None -> default | Some value -> value

let host = env "POSTER_MONGO_HOST" "oracle-vm"
let port = env "POSTER_MONGO_PORT" "27017" |> int_of_string

let db =
  Printf.sprintf "ent_ocaml_bulk_e2e_%d_%d" (Unix.getpid ()) (Random.bits ())

let post_entity =
  Ent_ocaml.
    {
      name = "Post";
      collection = "posts";
      fields =
        [
          {
            name = "id";
            storage_key = "id";
            typ = String;
            required = true;
            unique = true;
            immutable = false;
            nillable = false;
            validators = [];
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
            name = Some "unique_post_id";
            fields = [ "id" ];
            edges = [];
            unique = true;
          };
          {
            name = Some "posts_by_id_body";
            fields = [ "id"; "body" ];
            edges = [];
            unique = false;
          };
        ];
    }

let create id user_id body views =
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
      orders = [ { field = "id"; direction = Asc } ];
      limit = None;
      offset = None;
    }

let query_user_1 =
  Ent_ocaml.
    {
      entity = post_entity;
      predicates = [ Has_edge_with ("user", [ Eq ("id", V_string "user_1") ]) ];
      select = [];
      orders = [ { field = "id"; direction = Asc } ];
      limit = None;
      offset = None;
    }

let query_after_post_1 =
  Ent_ocaml.Query.make post_entity
  |> Ent_ocaml.Query.after ~field:"id" ~direction:Ent_ocaml.Asc
       (Ent_ocaml.V_string "post_1")
  |> Ent_ocaml.Query.limit 1

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

let run_flow client =
  let open Ent_ocaml.Result_syntax in
  let ctx = Ent_ocaml_mongo.create ~client { database = db } in
  let* () = Ent_ocaml_mongo.ensure_indexes ctx [ post_entity ] in
  let* docs =
    Ent_ocaml_mongo.insert_many_values ctx
      [
        create "post_1" "user_1" "first" 10L;
        create "post_2" "user_2" "second" 20L;
      ]
  in
  assert_true "bulk insert returns docs" (List.length docs = 2);
  let* found = Ent_ocaml_mongo.find ctx query_all in
  assert_true "bulk insert persisted rows" (List.length found = 2);
  let* page = Ent_ocaml_mongo.find ctx query_after_post_1 in
  assert_true "seek pagination returns next row"
    (match page with
    | [ doc ] -> Bson.get_string (Bson.get_element "id" doc) = "post_2"
    | _ -> false);
  let* user_posts = Ent_ocaml_mongo.find ctx query_user_1 in
  assert_true "edge predicate returns user posts" (List.length user_posts = 1);
  let* sum_value =
    Ent_ocaml_mongo.aggregate ctx (Ent_ocaml.Aggregate.sum "views" query_user_1)
  in
  assert_int64_value "aggregate sum returns user views" 10L sum_value;
  let* avg_value =
    Ent_ocaml_mongo.aggregate ctx (Ent_ocaml.Aggregate.avg "views" query_all)
  in
  assert_float_value "aggregate avg returns all views" 15.0 avg_value;
  let* grouped =
    Ent_ocaml_mongo.group ctx
      (Ent_ocaml.Aggregate.group_by "user_id"
         (Ent_ocaml.Aggregate.sum "views" query_all))
  in
  assert_true "group aggregate returns user_1 views"
    (group_value "user_1" grouped = Some 10L);
  assert_true "group aggregate returns user_2 views"
    (group_value "user_2" grouped = Some 20L);
  let* () =
    Ent_ocaml_mongo.upsert_one ctx (upsert "post_3" "user_1" "third")
  in
  let* found_after_insert = Ent_ocaml_mongo.find ctx query_all in
  assert_true "upsert inserts missing row" (List.length found_after_insert = 3);
  let* () =
    Ent_ocaml_mongo.upsert_one ctx
      (upsert "post_3" "user_1" "third updated")
  in
  let* user_posts_after_upsert = Ent_ocaml_mongo.find ctx query_user_1 in
  assert_true "upsert updates existing row"
    (List.exists
       (fun doc ->
         Bson.get_string (Bson.get_element "body" doc) = "third updated")
       user_posts_after_upsert);
  match
    Ent_ocaml_mongo.insert_many_values ctx
      [
        create "post_2" "user_2" "duplicate" 20L;
        create "post_4" "user_1" "fourth" 40L;
      ]
  with
  | Error (`Constraint _) -> (
      assert_true "bulk duplicate maps to constraint" true;
      match Ent_ocaml_mongo.insert_many_values ctx [ invalid_create_missing_body ] with
      | Error (`Bad_query message) ->
          assert_true "bulk create validates required fields"
            (message = "missing required field: body");
          Ok ()
      | Error error -> Error error
      | Ok _ ->
          Error (`Bad_query "invalid bulk create unexpectedly succeeded"))
  | Error error -> Error error
  | Ok _ -> Error (`Constraint "duplicate bulk insert succeeded")

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
