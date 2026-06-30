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
        ];
      edges = [];
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

let create id body =
  Ent_ocaml.
    {
      entity = post_entity;
      op = Create;
      predicates = [];
      set = [ ("id", V_string id); ("body", V_string body) ];
      clear = [];
      add = [];
    }

let invalid_create_missing_body =
  Ent_ocaml.
    {
      entity = post_entity;
      op = Create;
      predicates = [];
      set = [ ("id", V_string "post_missing_body") ];
      clear = [];
      add = [];
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

let assert_true label condition =
  if condition then Printf.printf "PASS %s\n%!" label
  else failwith ("FAIL " ^ label)

let cleanup client =
  Mongo_eio.direct_run_command client db [ ("dropDatabase", Bson.create_int32 1l) ]
  |> Result.map (fun _ -> ())

let run_flow client =
  let ctx = Ent_ocaml_mongo.create ~client { database = db } in
  match Ent_ocaml_mongo.ensure_indexes ctx [ post_entity ] with
  | Error error -> Error error
  | Ok () -> (
      match
        Ent_ocaml_mongo.insert_many_values ctx
          [ create "post_1" "first"; create "post_2" "second" ]
      with
      | Error error -> Error error
      | Ok docs -> (
          assert_true "bulk insert returns docs" (List.length docs = 2);
          match Ent_ocaml_mongo.find ctx query_all with
          | Error error -> Error error
          | Ok found -> (
              assert_true "bulk insert persisted rows" (List.length found = 2);
              match
                Ent_ocaml_mongo.insert_many_values ctx
                  [ create "post_2" "duplicate"; create "post_3" "third" ]
              with
              | Error (`Constraint _) -> (
                  assert_true "bulk duplicate maps to constraint" true;
                  match
                    Ent_ocaml_mongo.insert_many_values ctx
                      [ invalid_create_missing_body ]
                  with
                  | Error (`Bad_query message) ->
                      assert_true "bulk create validates required fields"
                        (message = "missing required field: body");
                      Ok ()
                  | Error error -> Error error
                  | Ok _ ->
                      Error
                        (`Bad_query "invalid bulk create unexpectedly succeeded"))
              | Error error -> Error error
              | Ok _ -> Error (`Constraint "duplicate bulk insert succeeded"))))

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
