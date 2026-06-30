let env name default =
  match Sys.getenv_opt name with Some "" | None -> default | Some value -> value

let host = env "POSTER_MONGO_HOST" "oracle-vm"
let port = env "POSTER_MONGO_PORT" "27017" |> int_of_string

let db =
  Printf.sprintf "ent_ocaml_bulk_e2e_%d_%d" (Unix.getpid ()) (Random.bits ())

let org_entity =
  Ent_ocaml.
    {
      name = "Org";
      collection = "orgs";
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
            name = "slug";
            storage_key = "slug";
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
      indexes = [];
    }

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
          {
            name = "org_id";
            storage_key = "org_id";
            typ = String;
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
            name = "org";
            target = "Org";
            direction = To;
            cardinality = One;
            required = false;
            storage_key = Some "org_id";
            join = None;
          };
          {
            name = "posts";
            target = "Post";
            direction = To;
            cardinality = Many;
            required = false;
            storage_key = Some "user_id";
            join = None;
          };
        ];
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

let tag_entity =
  Ent_ocaml.
    {
      name = "Tag";
      collection = "tags";
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
            name = "name";
            storage_key = "name";
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
      indexes = [];
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
            join = None;
          };
          {
            name = "tags";
            target = "Tag";
            direction = To;
            cardinality = Many;
            required = false;
            storage_key = None;
            join =
              Some
                {
                  collection = "post_tags";
                  source_key = "post_id";
                  target_key = "tag_id";
                };
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

let create_org id slug =
  Ent_ocaml.
    {
      entity = org_entity;
      op = Create;
      predicates = [];
      set = [ ("id", V_string id); ("slug", V_string slug) ];
      clear = [];
      add = [];
      on_insert = [];
    }

let create_user id username =
  let org_id = if id = "user_1" then "org_1" else "org_2" in
  Ent_ocaml.
    {
      entity = user_entity;
      op = Create;
      predicates = [];
      set =
        [
          ("id", V_string id);
          ("username", V_string username);
          ("org_id", V_string org_id);
        ];
      clear = [];
      add = [];
      on_insert = [];
    }

let create_tag id name =
  Ent_ocaml.
    {
      entity = tag_entity;
      op = Create;
      predicates = [];
      set = [ ("id", V_string id); ("name", V_string name) ];
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

let bson_doc fields =
  List.fold_left
    (fun acc (name, value) -> Bson.add_element name value acc)
    Bson.empty fields

let join_doc post_id tag_id =
  bson_doc
    [
      ("post_id", Bson.create_string post_id);
      ("tag_id", Bson.create_string tag_id);
    ]

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
      orders = [ Order.field ~direction:Asc "id" ];
      limit = None;
      offset = None;
    }

let query_user_1 =
  Ent_ocaml.
    {
      entity = post_entity;
      predicates = [ Has_edge_with ("user", [ Eq ("id", V_string "user_1") ]) ];
      select = [];
      orders = [ Order.field ~direction:Asc "id" ];
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
  Ent_ocaml.Edge_query.make ~as_:"author" ~edge:"user" ~target:user_entity
    query_user_1

let query_user_2 =
  Ent_ocaml.Query.make user_entity
    ~where:Ent_ocaml.[ Eq ("id", V_string "user_2") ]

let query_posts_from_user =
  Ent_ocaml.Edge_query.make ~as_:"posts" ~edge:"posts" ~target:post_entity
    ~target_query:
      Ent_ocaml.
        {
          entity = post_entity;
          predicates = [];
          select = [];
          orders = [ Order.field ~direction:Asc "id" ];
          limit = None;
          offset = None;
        }
    query_user_2

let query_latest_filtered_post_from_user =
  Ent_ocaml.Edge_query.make ~as_:"latest_post" ~edge:"posts" ~target:post_entity
    ~target_query:
      Ent_ocaml.
        {
          entity = post_entity;
          predicates = [ Neq ("body", V_string "second") ];
          select = [];
          orders = [ Order.field ~direction:Desc "id" ];
          limit = Some 1;
          offset = None;
        }
    query_user_2

let query_tags_from_posts =
  Ent_ocaml.Edge_query.make ~as_:"tags" ~edge:"tags" ~target:tag_entity
    ~target_query:
      Ent_ocaml.
        {
          entity = tag_entity;
          predicates = [ Neq ("name", V_string "skip") ];
          select = [];
          orders = [ Order.field ~direction:Asc "id" ];
          limit = Some 2;
          offset = None;
        }
    query_all

let query_posts_with_any_tag =
  Ent_ocaml.
    {
      query_all with
      predicates = [ Has_edge "tags" ];
      orders = [ Order.field ~direction:Asc "id" ];
    }

let query_posts_with_ocaml_tag =
  Ent_ocaml.
    {
      query_all with
      predicates = [ Has_edge_with ("tags", [ Eq ("id", V_string "tag_ocaml") ]) ];
      orders = [ Order.field ~direction:Asc "id" ];
    }

let query_posts_with_ocaml_tag_name =
  Ent_ocaml.
    {
      query_all with
      predicates =
        [
          Has_edge_with_target
            {
              edge = "tags";
              target = tag_entity;
              predicates = [ Eq ("name", V_string "ocaml") ];
            };
        ];
      orders = [ Order.field ~direction:Asc "id" ];
    }

let query_posts_by_tag_count =
  Ent_ocaml.
    {
      query_all with
      orders =
        [
          Order.edge_count ~edge:"tags" ~target:tag_entity ~direction:Desc ();
          Order.field ~direction:Asc "id";
        ];
      limit = Some 1;
    }

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

let query_id id =
  Ent_ocaml.
    {
      query_all with
      predicates = [ Eq ("id", V_string id) ];
      limit = Some 1;
    }

let string_contains haystack needle =
  let haystack = String.lowercase_ascii haystack in
  let needle = String.lowercase_ascii needle in
  let haystack_len = String.length haystack in
  let needle_len = String.length needle in
  let rec loop index =
    index + needle_len <= haystack_len
    &&
    (String.sub haystack index needle_len = needle || loop (index + 1))
  in
  needle_len = 0 || loop 0

let transaction_not_supported message =
  List.exists
    (string_contains message)
    [
      "transaction numbers are only allowed";
      "transaction";
      "replica set";
      "not supported";
    ]

let check_transaction_rollback ctx =
  let open Ent_ocaml.Result_syntax in
  let tx_id = "tx_rollback" in
  match
    Ent_ocaml_mongo.transaction ctx (fun tx ->
        let* _ =
          Ent_ocaml_mongo.insert_values tx
            (create tx_id "user_1" "rolled back" 1L)
        in
        Error (`Bad_query "force rollback"))
  with
  | Error (`Bad_query "force rollback") ->
      let* rows = Ent_ocaml_mongo.find ctx (query_id tx_id) in
      assert_true "transaction rollback removes inserted row" (rows = []);
      Ok ()
  | Error (`Backend message) when transaction_not_supported message ->
      Printf.printf
        "SKIP transaction rollback unsupported by Mongo deployment: %s\n%!"
        message;
      Ok ()
  | Error _ as error -> error
  | Ok _ -> Error (`Bad_query "expected transaction rollback error")

let check_index_drift ctx =
  let open Ent_ocaml.Result_syntax in
  let* checks =
    Ent_ocaml_mongo.check_indexes ctx [ org_entity; user_entity; post_entity ]
  in
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

let check_collection_validator_drift ctx =
  let open Ent_ocaml.Result_syntax in
  let* checks =
    Ent_ocaml_mongo.check_collection_validators ctx
      [ org_entity; user_entity; post_entity ]
  in
  assert_true "collection validator check passes"
    (List.for_all Ent_ocaml_mongo.collection_validator_check_ok checks);
  let drift_entity =
    Ent_ocaml.
      {
        post_entity with
        fields =
          {
            name = "unexpected_required";
            storage_key = "unexpected_required";
            typ = String;
            required = true;
            unique = false;
            immutable = false;
            nillable = false;
            validators = [];
            sensitive = false;
            deprecated = None;
            comment = None;
          }
          :: post_entity.fields;
      }
  in
  let* drift_checks =
    Ent_ocaml_mongo.check_collection_validators ctx [ drift_entity ]
  in
  assert_true "collection validator drift check reports mismatch"
    (List.exists
       (fun (check : Ent_ocaml_mongo.collection_validator_check) ->
         match check.validator_status with
         | Ent_ocaml_mongo.Validator_mismatched _ -> true
         | Ent_ocaml_mongo.Validator_present | Ent_ocaml_mongo.Validator_missing ->
             false)
       drift_checks);
  match Ent_ocaml_mongo.verify_collection_validators ctx [ drift_entity ] with
  | Ok () ->
      Error (`Bad_query "expected collection validator drift verification failure")
  | Error (`Bad_schema _) -> Ok ()
  | Error _ as error -> error

let run_flow client =
  let open Ent_ocaml.Result_syntax in
  let ctx = Ent_ocaml_mongo.create ~client { database = db } in
  let* () =
    Ent_ocaml_mongo.ensure_collection_validators ctx
      [ org_entity; user_entity; tag_entity; post_entity ]
  in
  let* () = check_collection_validator_drift ctx in
  let* () =
    Ent_ocaml_mongo.ensure_indexes ctx
      [ org_entity; user_entity; tag_entity; post_entity ]
  in
  let* () = check_index_drift ctx in
  let* () = check_partial_index client in
  let* _orgs =
    Ent_ocaml_mongo.insert_many_values ctx
      [ create_org "org_1" "engineering"; create_org "org_2" "marketing" ]
  in
  let* _users =
    Ent_ocaml_mongo.insert_many_values ctx
      [ create_user "user_1" "alice"; create_user "user_2" "bob" ]
  in
  let* _tags =
    Ent_ocaml_mongo.insert_many_values ctx
      [
        create_tag "tag_ocaml" "ocaml";
        create_tag "tag_mongo" "mongo";
        create_tag "tag_skip" "skip";
      ]
  in
  let* docs =
    Ent_ocaml_mongo.insert_many_values ctx
      [
        create "post_1" "user_1" "first" 10L;
        create "post_2" "user_2" "second" 20L;
        create "post_2b" "user_2" "second-b" 20L;
      ]
  in
  let* _ =
    match
      Mongo_eio.direct_insert_many client ~db ~collection:"post_tags"
        ~options:Mongo_crud.default_insert
        [
          join_doc "post_1" "tag_ocaml";
          join_doc "post_2" "tag_mongo";
          join_doc "post_2" "tag_skip";
          join_doc "post_2b" "tag_ocaml";
        ]
    with
    | Ok result -> Ok result
    | Error error -> Error (`Backend (Mongo_error.to_string error))
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
              Order.field ~as_:"ordered_views" ~direction:Desc "views";
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
  let* entql_pinned_posts =
    match
      Ent_ocaml.Query.make post_entity
      |> Ent_ocaml.Entql.where {|metadata.flags.pinned == true|}
    with
    | Ok query -> Ent_ocaml_mongo.find ctx query
    | Error _ as error -> error
  in
  assert_true "entql json path predicate returns pinned post"
    (match entql_pinned_posts with
    | [ doc ] -> Bson.get_string (Bson.get_element "_id" doc) = "post_2"
    | _ -> false);
  let* entql_priority_posts =
    match
      Ent_ocaml.Query.make post_entity
      |> Ent_ocaml.Entql.where {|metadata.priority >= 20|}
    with
    | Ok query -> Ent_ocaml_mongo.find ctx query
    | Error _ as error -> error
  in
  assert_true "entql json path comparison returns priority posts"
    (List.map
       (fun doc -> Bson.get_string (Bson.get_element "_id" doc))
       entql_priority_posts
    = [ "post_2"; "post_2b" ]);
  let* priority_posts =
    Ent_ocaml_mongo.find ctx
      Ent_ocaml.
        {
          entity = post_entity;
          predicates = [];
          select = [];
          orders = [ Order.field ~direction:Desc "metadata.priority" ];
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
              Order.field ~as_:"priority" ~direction:Desc "metadata.priority";
            ];
          limit = Some 1;
        }
  in
  assert_true "selected json order value returns sort term"
    (selected_json_order_value = Some (Ent_ocaml.V_int64 30L));
  let score_expression =
    Ent_ocaml.
      V_doc
        [
          ( "$multiply",
            V_list [ V_string "$views"; V_string "$metadata.priority" ] );
        ]
  in
  let custom_order_query =
    Ent_ocaml.
      {
        query_all with
        orders =
          [
            Ent_ocaml_mongo.Order.expression ~name:"view_priority_score"
              ~direction:Desc score_expression;
          ];
        limit = Some 1;
      }
  in
  let* custom_ordered_posts = Ent_ocaml_mongo.find ctx custom_order_query in
  assert_true "mongo custom expression order returns highest score"
    (match custom_ordered_posts with
    | [ doc ] -> Bson.get_string (Bson.get_element "_id" doc) = "post_2b"
    | _ -> false);
  let* selected_custom_order_value =
    Ent_ocaml_mongo.value ctx
      Ent_ocaml.
        {
          custom_order_query with
          orders =
            [
              Ent_ocaml_mongo.Order.expression ~name:"view_priority_score"
                ~direction:Desc ~as_:"score" score_expression;
            ];
        }
  in
  assert_int64_value "selected mongo custom order value returns expression" 600L
    selected_custom_order_value;
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
  let edge_order_query =
    Ent_ocaml.
      {
        query_all with
        orders =
          [
            Order.edge_field ~edge:"user" ~target:user_entity
              ~direction:Desc "username";
          ];
        limit = Some 1;
      }
  in
  let* edge_ordered_posts = Ent_ocaml_mongo.find ctx edge_order_query in
  assert_true "edge field order returns bob post first"
    (match edge_ordered_posts with
    | [ doc ] -> Bson.get_string (Bson.get_element "user_id" doc) = "user_2"
    | _ -> false);
  let* selected_edge_order_value =
    Ent_ocaml_mongo.value ctx
      Ent_ocaml.
        {
          query_all with
          orders =
            [
              Order.edge_field ~edge:"user" ~target:user_entity
                ~direction:Asc ~as_:"author_username" "username";
            ];
          limit = Some 1;
        }
  in
  assert_true "selected edge order value returns related field"
    (selected_edge_order_value = Some (Ent_ocaml.V_string "alice"));
  let edge_count_order_query =
    Ent_ocaml.
      {
        entity = user_entity;
        predicates = [];
        select = [];
        orders =
          [
            Order.edge_count ~edge:"posts" ~target:post_entity
              ~direction:Desc ();
          ];
        limit = Some 1;
        offset = None;
      }
  in
  let* edge_count_ordered_users =
    Ent_ocaml_mongo.find ctx edge_count_order_query
  in
  assert_true "edge count order returns user with most posts"
    (match edge_count_ordered_users with
    | [ doc ] -> Bson.get_string (Bson.get_element "_id" doc) = "user_2"
    | _ -> false);
  let* selected_edge_count_order_value =
    Ent_ocaml_mongo.value ctx
      Ent_ocaml.
        {
          edge_count_order_query with
          orders =
            [
              Order.edge_count ~edge:"posts" ~target:post_entity
                ~direction:Desc ~as_:"post_count" ();
            ];
        }
  in
  assert_int64_value "selected edge count order value returns count" 2L
    selected_edge_count_order_value;
  let* join_edge_count_ordered_posts =
    Ent_ocaml_mongo.find ctx query_posts_by_tag_count
  in
  assert_true "join edge count order returns most tagged post"
    (match join_edge_count_ordered_posts with
    | [ doc ] -> Bson.get_string (Bson.get_element "_id" doc) = "post_2"
    | _ -> false);
  let* selected_join_edge_count_order_value =
    Ent_ocaml_mongo.value ctx
      Ent_ocaml.
        {
          query_posts_by_tag_count with
          orders =
            [
              Order.edge_count ~edge:"tags" ~target:tag_entity ~direction:Desc
                ~as_:"tag_count" ();
              Order.field ~direction:Asc "id";
            ];
        }
  in
  assert_int64_value "selected join edge count order value returns count" 2L
    selected_join_edge_count_order_value;
  let* entql_user_posts =
    match
      Ent_ocaml.Query.make post_entity
      |> Ent_ocaml.Entql.where {|user.id == "user_1"|}
    with
    | Ok query -> Ent_ocaml_mongo.find ctx query
    | Error _ as error -> error
  in
  assert_true "entql edge path returns user posts"
    (List.length entql_user_posts = 1);
  let* entql_alice_posts =
    match
      Ent_ocaml.Query.make post_entity
      |> Ent_ocaml.Entql.where ~targets:[ user_entity ]
           {|user.username == "alice"|}
    with
    | Ok query -> Ent_ocaml_mongo.find ctx query
    | Error _ as error -> error
  in
  assert_true "entql target edge path returns alice posts"
    (match entql_alice_posts with
    | [ doc ] -> Bson.get_string (Bson.get_element "_id" doc) = "post_1"
    | _ -> false);
  let* entql_engineering_posts =
    match
      Ent_ocaml.Query.make post_entity
      |> Ent_ocaml.Entql.where ~targets:[ user_entity; org_entity ]
           {|user.org.id == "org_1"|}
    with
    | Ok query -> Ent_ocaml_mongo.find ctx query
    | Error _ as error -> error
  in
  assert_true "entql nested edge id path returns engineering posts"
    (match entql_engineering_posts with
    | [ doc ] -> Bson.get_string (Bson.get_element "_id" doc) = "post_1"
    | _ -> false);
  let* entql_engineering_slug_posts =
    match
      Ent_ocaml.Query.make post_entity
      |> Ent_ocaml.Entql.where ~targets:[ user_entity; org_entity ]
           {|user.org.slug == "engineering"|}
    with
    | Ok query -> Ent_ocaml_mongo.find ctx query
    | Error _ as error -> error
  in
  assert_true "entql nested edge target field path returns engineering posts"
    (match entql_engineering_slug_posts with
    | [ doc ] -> Bson.get_string (Bson.get_element "_id" doc) = "post_1"
    | _ -> false);
  let* alice_post_count =
    Ent_ocaml_mongo.count ctx
      (Ent_ocaml.Query.make post_entity
         ~where:
           Ent_ocaml.
             [
               Has_edge_with_target
                 {
                   edge = "user";
                   target = user_entity;
                   predicates = [ Eq ("username", V_string "alice") ];
                 };
             ])
  in
  assert_true "target edge count returns alice posts" (alice_post_count = 1);
  let* engineering_post_count =
    match
      Ent_ocaml.Query.make post_entity
      |> Ent_ocaml.Entql.where ~targets:[ user_entity; org_entity ]
           {|user.org.slug == "engineering"|}
    with
    | Ok query -> Ent_ocaml_mongo.count ctx query
    | Error _ as error -> error
  in
  assert_true "nested target edge count returns engineering posts"
    (engineering_post_count = 1);
  let* alice_posts =
    Ent_ocaml_mongo.find ctx
      (Ent_ocaml.Query.make post_entity
         ~where:
           Ent_ocaml.
             [
               Has_edge_with_target
                 {
                   edge = "user";
                   target = user_entity;
                   predicates = [ Eq ("username", V_string "alice") ];
                 };
             ])
  in
  assert_true "target edge predicate returns alice posts"
    (match alice_posts with
    | [ doc ] -> Bson.get_string (Bson.get_element "_id" doc) = "post_1"
    | _ -> false);
  let* tagged_posts = Ent_ocaml_mongo.find ctx query_posts_with_any_tag in
  assert_true "join edge predicate returns tagged posts"
    (List.map (fun doc -> Bson.get_string (Bson.get_element "_id" doc)) tagged_posts
    = [ "post_1"; "post_2"; "post_2b" ]);
  let* ocaml_tagged_posts =
    Ent_ocaml_mongo.find ctx query_posts_with_ocaml_tag
  in
  assert_true "join edge id predicate returns tagged posts"
    (List.map
       (fun doc -> Bson.get_string (Bson.get_element "_id" doc))
       ocaml_tagged_posts
    = [ "post_1"; "post_2b" ]);
  let* ocaml_tag_name_posts =
    Ent_ocaml_mongo.find ctx query_posts_with_ocaml_tag_name
  in
  assert_true "join target edge predicate returns tagged posts"
    (List.map
       (fun doc -> Bson.get_string (Bson.get_element "_id" doc))
       ocaml_tag_name_posts
    = [ "post_1"; "post_2b" ]);
  let* entql_ocaml_tag_posts =
    match
      Ent_ocaml.Query.make post_entity
      |> Ent_ocaml.Entql.where ~targets:[ tag_entity ] {|tags.name == "ocaml"|}
    with
    | Ok query ->
        Ent_ocaml_mongo.find ctx
          Ent_ocaml.{ query with orders = [ Order.field ~direction:Asc "id" ] }
    | Error _ as error -> error
  in
  assert_true "entql join target edge path returns tagged posts"
    (List.map
       (fun doc -> Bson.get_string (Bson.get_element "_id" doc))
       entql_ocaml_tag_posts
    = [ "post_1"; "post_2b" ]);
  let* ocaml_tag_count =
    match
      Ent_ocaml.Query.make post_entity
      |> Ent_ocaml.Entql.where ~targets:[ tag_entity ] {|tags.name == "ocaml"|}
    with
    | Ok query -> Ent_ocaml_mongo.count ctx query
    | Error _ as error -> error
  in
  assert_true "join target edge count returns tagged posts"
    (ocaml_tag_count = 2);
  assert_true "named edge alias preserved"
    (query_user_from_posts.Ent_ocaml.edge_alias = Some "author");
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
  let* traversed_posts =
    Ent_ocaml_mongo.traverse_as ctx query_posts_from_user ~decode:(fun doc ->
        Ok (Bson.get_string (Bson.get_element "_id" doc)))
  in
  assert_true "stored to-many traversal returns posts"
    (traversed_posts = [ "post_2"; "post_2b" ]);
  let* loaded_posts =
    Ent_ocaml_mongo.load_edge_as ctx query_posts_from_user
      ~decode_source:(fun doc ->
        Ok (Bson.get_string (Bson.get_element "username" doc)))
      ~decode_target:(fun doc ->
        Ok (Bson.get_string (Bson.get_element "_id" doc)))
  in
  assert_true "stored to-many eager load returns source and posts"
    (loaded_posts = [ ("bob", Some "post_2"); ("bob", Some "post_2b") ]);
  let* traversed_filtered_posts =
    Ent_ocaml_mongo.traverse_as ctx query_latest_filtered_post_from_user
      ~decode:(fun doc -> Ok (Bson.get_string (Bson.get_element "_id" doc)))
  in
  assert_true "stored to-many traversal applies target query"
    (traversed_filtered_posts = [ "post_2b" ]);
  let* loaded_filtered_posts =
    Ent_ocaml_mongo.load_edge_as ctx query_latest_filtered_post_from_user
      ~decode_source:(fun doc ->
        Ok (Bson.get_string (Bson.get_element "username" doc)))
      ~decode_target:(fun doc ->
        Ok (Bson.get_string (Bson.get_element "_id" doc)))
  in
  assert_true "stored to-many eager load applies target query"
    (loaded_filtered_posts = [ ("bob", Some "post_2b") ]);
  let* traversed_tags =
    Ent_ocaml_mongo.traverse_as ctx query_tags_from_posts ~decode:(fun doc ->
        Ok (Bson.get_string (Bson.get_element "name" doc)))
  in
  assert_true "join to-many traversal returns tags"
    (traversed_tags = [ "mongo"; "ocaml" ]);
  let* loaded_tags =
    Ent_ocaml_mongo.load_edge_as ctx query_tags_from_posts
      ~decode_source:(fun doc ->
        Ok (Bson.get_string (Bson.get_element "_id" doc)))
      ~decode_target:(fun doc ->
        Ok (Bson.get_string (Bson.get_element "name" doc)))
  in
  assert_true "join to-many eager load returns source and tags"
    (loaded_tags
    = [
        ("post_1", Some "ocaml");
        ("post_2", Some "mongo");
        ("post_2b", Some "ocaml");
      ]);
  let editor_edge =
    Ent_ocaml.Edge_query.make ~as_:"editor" ~edge:"user" ~target:user_entity
      query_user_1
  in
  let* editor_users =
    Ent_ocaml_mongo.load_edge_as ctx editor_edge
      ~decode_source:(fun doc ->
        Ok (Bson.get_string (Bson.get_element "body" doc)))
      ~decode_target:(fun doc ->
        Ok (Bson.get_string (Bson.get_element "_id" doc)))
  in
  let groups =
    [
      Ent_ocaml.Edge_load.group_of_pairs query_user_from_posts loaded_users;
      Ent_ocaml.Edge_load.group_of_pairs editor_edge editor_users;
    ]
  in
  assert_true "multi-edge named groups preserve aliases"
    (match groups with
    | [
     { Ent_ocaml.loaded_group_name = "author"; loaded_group_rows = [ author ] };
     { Ent_ocaml.loaded_group_name = "editor"; loaded_group_rows = [ editor ] };
    ] ->
        author.loaded_target = Some "user_1"
        && editor.loaded_target = Some "user_1"
    | _ -> false);
  let* sum =
    Ent_ocaml_mongo.aggregate ctx (Ent_ocaml.Aggregate.sum "views" query_user_1)
  in
  assert_int64_value "aggregate sum returns user views" 10L sum;
  let* alice_target_sum =
    Ent_ocaml_mongo.aggregate ctx
      (Ent_ocaml.Aggregate.sum "views"
         (Ent_ocaml.Query.make post_entity
            ~where:
              Ent_ocaml.
                [
                  Has_edge_with_target
                    {
                      edge = "user";
                      target = user_entity;
                      predicates = [ Eq ("username", V_string "alice") ];
                    };
                ]))
  in
  assert_int64_value "target edge aggregate sum returns user views" 10L
    alice_target_sum;
  let* engineering_sum =
    match
      Ent_ocaml.Query.make post_entity
      |> Ent_ocaml.Entql.where ~targets:[ user_entity; org_entity ]
           {|user.org.slug == "engineering"|}
    with
    | Ok query ->
        Ent_ocaml_mongo.aggregate ctx (Ent_ocaml.Aggregate.sum "views" query)
    | Error _ as error -> error
  in
  assert_int64_value "nested target edge aggregate sum returns org views" 10L
    engineering_sum;
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
  let* tagged_scan =
    match
      Ent_ocaml.Query.make post_entity
      |> Ent_ocaml.Entql.where ~targets:[ tag_entity ] {|tags.name == "ocaml"|}
    with
    | Ok query ->
        Ent_ocaml_mongo.aggregate_scan ctx
          (Ent_ocaml.Aggregate.scan
             Ent_ocaml.Aggregate.[ count_as "posts"; sum_as "views" "views" ]
             query)
    | Error _ as error -> error
  in
  assert_true "join target aggregate scan returns count and sum"
    (assoc_int64 "posts" tagged_scan = Some 2L
    && assoc_int64 "views" tagged_scan = Some 30L);
  let* grouped =
    Ent_ocaml_mongo.group ctx
      (Ent_ocaml.Aggregate.group_by "user_id"
         (Ent_ocaml.Aggregate.sum "views" query_all))
  in
  assert_true "group aggregate returns user_1 views"
    (group_value "user_1" grouped = Some 10L);
  assert_true "group aggregate returns user_2 views"
    (group_value "user_2" grouped = Some 40L);
  let* grouped_tagged =
    match
      Ent_ocaml.Query.make post_entity
      |> Ent_ocaml.Entql.where ~targets:[ tag_entity ] {|tags.name == "ocaml"|}
    with
    | Ok query ->
        Ent_ocaml_mongo.group ctx
          (Ent_ocaml.Aggregate.group_by "user_id"
             (Ent_ocaml.Aggregate.sum "views" query))
    | Error _ as error -> error
  in
  assert_true "join target group aggregate returns user_1 views"
    (group_value "user_1" grouped_tagged = Some 10L);
  assert_true "join target group aggregate returns user_2 views"
    (group_value "user_2" grouped_tagged = Some 20L);
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
  let* () = check_transaction_rollback ctx in
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
