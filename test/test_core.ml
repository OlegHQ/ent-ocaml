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
            immutable = true;
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
      global_id = false;
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
            immutable = true;
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
      indexes = [];
      global_id = false;
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
            immutable = true;
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
            name = "body";
            storage_key = "body";
            typ = String;
            required = false;
            unique = false;
            immutable = false;
            nillable = false;
            validators =
              [
                (function
                | V_string value ->
                    if value = "" then Error "must not be empty" else Ok ()
                | _ -> Error "expected string");
              ];
            sensitive = true;
            deprecated = Some "use summary";
            comment = Some "Post body text";
          };
          {
            name = "published_at_ms";
            storage_key = "published_at_ms";
            typ = Option Int64;
            required = false;
            unique = false;
            immutable = false;
            nillable = true;
            validators = [];
            sensitive = false;
            deprecated = None;
            comment = None;
          };
          {
            name = "metadata";
            storage_key = "meta";
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
      ];
      indexes = [];
      global_id = false;
    }

let query ?(predicates = []) ?(select = []) ?(orders = []) ?limit ?offset () =
  Ent_ocaml.{ entity = post_entity; predicates; select; orders; limit; offset }

let mutation ?(predicates = []) ?(set = []) ?(clear = []) ?(add = [])
    ?(on_insert = []) op =
  Ent_ocaml.
    { entity = post_entity; op; predicates; set; clear; add; on_insert }

let test_error_to_string () =
  Alcotest.(check string)
    "not found" "not found" (Ent_ocaml.error_to_string `Not_found)

let test_validate_create_missing_required () =
  match
    Ent_ocaml.validate_mutation
      (mutation Ent_ocaml.Create
         ~set:Ent_ocaml.[ ("id", V_string "post_1") ])
  with
  | Ok () -> Alcotest.fail "expected missing required field"
  | Error (`Bad_query message) ->
      Alcotest.(check string)
        "message" "missing required field: user_id" message
  | Error error -> Alcotest.fail (Ent_ocaml.error_to_string error)

let test_validate_unknown_field () =
  match
    Ent_ocaml.validate_mutation
      (mutation Ent_ocaml.Create
         ~set:
           Ent_ocaml.
             [
               ("id", V_string "post_1");
               ("user_id", V_string "user_1");
               ("missing", V_string "bad");
             ])
  with
  | Ok () -> Alcotest.fail "expected unknown field"
  | Error (`Bad_query message) ->
      Alcotest.(check string)
        "message" "mutation field not found: missing" message
  | Error error -> Alcotest.fail (Ent_ocaml.error_to_string error)

let test_validate_immutable_update () =
  match
    Ent_ocaml.validate_mutation
      (mutation Ent_ocaml.Update_one
         ~set:Ent_ocaml.[ ("id", V_string "post_2") ])
  with
  | Ok () -> Alcotest.fail "expected immutable update error"
  | Error (`Bad_query message) ->
      Alcotest.(check string)
        "message" "immutable field cannot be updated: id" message
  | Error error -> Alcotest.fail (Ent_ocaml.error_to_string error)

let test_validate_field_validator () =
  match
    Ent_ocaml.validate_mutation
      (mutation Ent_ocaml.Update_one
         ~set:Ent_ocaml.[ ("body", V_string "") ])
  with
  | Ok () -> Alcotest.fail "expected field validation error"
  | Error (`Bad_query message) ->
      Alcotest.(check string)
        "message" "validation failed for field body: must not be empty" message
  | Error error -> Alcotest.fail (Ent_ocaml.error_to_string error)

let test_validate_upsert_required_on_insert () =
  let upsert =
    mutation Ent_ocaml.Upsert_one
      ~predicates:Ent_ocaml.[ Eq ("id", V_string "post_1") ]
      ~set:Ent_ocaml.[ ("body", V_string "updated") ]
      ~on_insert:
        Ent_ocaml.[ ("id", V_string "post_1"); ("user_id", V_string "user_1") ]
  in
  match Ent_ocaml.validate_mutation upsert with
  | Ok () -> ()
  | Error error -> Alcotest.fail (Ent_ocaml.error_to_string error)

let test_validate_upsert_rejects_immutable_set () =
  match
    Ent_ocaml.validate_mutation
      (mutation Ent_ocaml.Upsert_one
         ~predicates:Ent_ocaml.[ Eq ("id", V_string "post_1") ]
         ~set:Ent_ocaml.[ ("id", V_string "post_2") ]
         ~on_insert:Ent_ocaml.[ ("user_id", V_string "user_1") ])
  with
  | Ok () -> Alcotest.fail "expected immutable upsert set error"
  | Error (`Bad_query message) ->
      Alcotest.(check string)
        "message" "immutable field cannot be updated: id" message
  | Error error -> Alcotest.fail (Ent_ocaml.error_to_string error)

let test_validate_update_rejects_on_insert () =
  match
    Ent_ocaml.validate_mutation
      (mutation Ent_ocaml.Update_one
         ~set:Ent_ocaml.[ ("body", V_string "updated") ]
         ~on_insert:Ent_ocaml.[ ("id", V_string "post_1") ])
  with
  | Ok () -> Alcotest.fail "expected update on_insert error"
  | Error (`Bad_query message) ->
      Alcotest.(check string)
        "message" "update mutation cannot have on_insert fields" message
  | Error error -> Alcotest.fail (Ent_ocaml.error_to_string error)

let test_query_pipeline_api () =
  let query =
    Ent_ocaml.Query.make post_entity
    |> Ent_ocaml.Query.where Ent_ocaml.(Eq ("user_id", V_string "user_1"))
    |> Ent_ocaml.Query.select [ "id"; "body" ]
    |> Ent_ocaml.Query.order_by
         Ent_ocaml.
           [
             Order.field ~direction:Asc "id"
             |> Query.order_value "ordered_id";
           ]
    |> Ent_ocaml.Query.limit 5
  in
  Alcotest.(check int) "predicates" 1 (List.length query.predicates);
  Alcotest.(check (list string)) "select" [ "id"; "body" ] query.select;
  Alcotest.(check (option string))
    "order value alias" (Some "ordered_id")
    (List.hd query.orders).value_alias;
  Alcotest.(check (option int)) "limit" (Some 5) query.limit

let test_query_seek_pagination () =
  let after_query =
    Ent_ocaml.Query.make post_entity
    |> Ent_ocaml.Query.after ~field:"id" ~direction:Ent_ocaml.Asc
         (Ent_ocaml.V_string "post_1")
  in
  Alcotest.(check bool)
    "after asc predicate" true
    (match after_query.predicates with
    | [ Ent_ocaml.Gt ("id", V_string "post_1") ] -> true
    | _ -> false);
  Alcotest.(check bool)
    "after adds order" true
    (match after_query.orders with
    | [ { Ent_ocaml.target = Field_order "id"; direction = Asc } ] -> true
    | _ -> false);
  let before_query =
    Ent_ocaml.Query.make post_entity
    |> Ent_ocaml.Query.order_by
         Ent_ocaml.[ Order.field ~direction:Desc "id" ]
    |> Ent_ocaml.Query.before ~field:"id" ~direction:Ent_ocaml.Desc
         (Ent_ocaml.V_string "post_2")
  in
  Alcotest.(check bool)
    "before desc predicate" true
    (match before_query.predicates with
    | [ Ent_ocaml.Gt ("id", V_string "post_2") ] -> true
    | _ -> false);
  Alcotest.(check int) "existing order preserved once" 1
    (List.length before_query.orders)

let test_query_composite_cursor_pagination () =
  let query =
    Ent_ocaml.Query.make post_entity
    |> Ent_ocaml.Query.after_cursor
         Ent_ocaml.
           [
             { field = "created_at_ms"; direction = Desc; value = V_int64 10L };
             { field = "id"; direction = Desc; value = V_string "post_2" };
           ]
  in
  Alcotest.(check int) "cursor orders" 2 (List.length query.orders);
  Alcotest.(check bool)
    "composite cursor predicate" true
    (match query.predicates with
    | [
        Ent_ocaml.Or
          [
            Lt ("created_at_ms", V_int64 10L);
            And
              [
                Eq ("created_at_ms", V_int64 10L);
                Lt ("id", V_string "post_2");
              ];
          ];
      ] ->
        true
    | _ -> false)

let test_result_syntax () =
  let open Ent_ocaml.Result_syntax in
  let result =
    let* left = Ok 2 in
    let+ right = Ok 3 in
    left + right
  in
  Alcotest.(check (result int string)) "result" (Ok 5) result

let test_privacy_rule_chain () =
  let query = Ent_ocaml.Query.make post_entity in
  let mutation =
    {
      Ent_ocaml.entity = post_entity;
      op = Create;
      predicates = [];
      set = Ent_ocaml.[ ("id", V_string "post_1") ];
      clear = [];
      add = [];
      on_insert = [];
    }
  in
  let open Ent_ocaml in
  (match Privacy.evaluate_query () [] query with
  | Ok () -> ()
  | Error error -> Alcotest.fail (error_to_string error));
  (match Privacy.evaluate_query () [ (fun () _ -> Skip) ] query with
  | Error (`Denied "privacy rule chain skipped") -> ()
  | Ok () -> Alcotest.fail "expected skipped query chain denial"
  | Error error -> Alcotest.fail (error_to_string error));
  (match
     Privacy.evaluate_query ()
       [ (fun () _ -> Skip); (fun () _ -> Allow); (fun () _ -> Deny "late") ]
       query
   with
  | Ok () -> ()
  | Error error -> Alcotest.fail (error_to_string error));
  (match Privacy.evaluate_mutation () [ (fun () _ -> Deny "no writes") ] mutation with
  | Error (`Denied "no writes") -> ()
  | Ok () -> Alcotest.fail "expected mutation denial"
  | Error error -> Alcotest.fail (error_to_string error))

let test_mutation_hook_chain () =
  let mutation =
    {
      Ent_ocaml.entity = post_entity;
      op = Update_one;
      predicates = [];
      set = [];
      clear = [];
      add = [];
      on_insert = [];
    }
  in
  let hook =
    {
      Ent_ocaml.wrap_mutation =
        (fun next ctx mutation ->
          let mutation =
            Ent_ocaml.Mutation.set Ent_ocaml.("status", V_string "hooked") mutation
          in
          next ctx mutation);
    }
  in
  let executor () mutation =
    Ok (List.assoc_opt "status" mutation.Ent_ocaml.set)
  in
  match Ent_ocaml.Hook.run_mutation [ hook ] executor () mutation with
  | Ok (Some (Ent_ocaml.V_string "hooked")) -> ()
  | Ok _ -> Alcotest.fail "expected rewritten mutation"
  | Error error -> Alcotest.fail (Ent_ocaml.error_to_string error)

let test_query_interceptor_chain () =
  let query = Ent_ocaml.Query.make post_entity in
  let interceptor =
    {
      Ent_ocaml.wrap_query =
        (fun next ctx query ->
          let query =
            Ent_ocaml.Query.where Ent_ocaml.(Eq ("status", V_string "draft")) query
          in
          next ctx query);
    }
  in
  let executor () (query : Ent_ocaml.query) =
    Ok (List.length query.Ent_ocaml.predicates)
  in
  match Ent_ocaml.Interceptor.run_query [ interceptor ] executor () query with
  | Ok 1 -> ()
  | Ok count -> Alcotest.failf "expected one predicate, got %d" count
  | Error error -> Alcotest.fail (Ent_ocaml.error_to_string error)

let test_edge_interceptor_chain () =
  let query = Ent_ocaml.Query.make post_entity in
  let edge_query =
    Ent_ocaml.Edge_query.make ~edge:"user" ~target:post_entity query
  in
  let interceptor =
    {
      Ent_ocaml.wrap_edge =
        (fun next ctx intercepted_edge_query ->
          let source =
            Ent_ocaml.Query.where
              (Ent_ocaml.Eq ("status", Ent_ocaml.V_string "edge-intercepted"))
              intercepted_edge_query.source
          in
          next ctx { intercepted_edge_query with source });
    }
  in
  let executor () (edge_query : Ent_ocaml.edge_query) =
    Ok (List.length edge_query.Ent_ocaml.source.predicates)
  in
  match Ent_ocaml.Edge_interceptor.run_edge [ interceptor ] executor () edge_query with
  | Ok 1 -> ()
  | Ok count -> Alcotest.failf "expected one edge predicate, got %d" count
  | Error error -> Alcotest.fail (Ent_ocaml.error_to_string error)

let test_edge_query_alias () =
  let edge_query =
    Ent_ocaml.Edge_query.make ~as_:"author" ~edge:"user" ~target:post_entity
      (Ent_ocaml.Query.make post_entity)
  in
  Alcotest.(check (option string))
    "edge alias" (Some "author") edge_query.edge_alias;
  Alcotest.(check string)
    "edge load name" "author" (Ent_ocaml.Edge_load.name edge_query);
  let loaded = Ent_ocaml.Edge_load.of_pair edge_query ("post", Some "user") in
  Alcotest.(check string) "loaded edge" "user" loaded.loaded_edge;
  Alcotest.(check (option string))
    "loaded alias" (Some "author") loaded.loaded_alias;
  Alcotest.(check string) "loaded name" "author" loaded.loaded_name;
  Alcotest.(check string) "loaded source" "post" loaded.loaded_source;
  Alcotest.(check (option string))
    "loaded target" (Some "user") loaded.loaded_target;
  let group = Ent_ocaml.Edge_load.group edge_query [ loaded ] in
  Alcotest.(check string) "loaded group edge" "user" group.loaded_group_edge;
  Alcotest.(check (option string))
    "loaded group alias" (Some "author") group.loaded_group_alias;
  Alcotest.(check string) "loaded group name" "author" group.loaded_group_name;
  Alcotest.(check int)
    "loaded group rows" 1 (List.length group.loaded_group_rows)

let test_edge_chain_builder () =
  let first =
    Ent_ocaml.Edge_query.make ~as_:"posts" ~edge:"posts" ~target:post_entity
      (Ent_ocaml.Query.make user_entity)
  in
  let chain =
    first
    |> Ent_ocaml.Edge_chain.start
    |> Ent_ocaml.Edge_chain.then_ ~as_:"labels" ~edge:"tags"
         ~target:org_entity
  in
  Alcotest.(check string)
    "chain source" "User"
    (Ent_ocaml.Edge_chain.source chain).Ent_ocaml.entity.name;
  Alcotest.(check string)
    "chain target" "Org" (Ent_ocaml.Edge_chain.target chain).name;
  Alcotest.(check string)
    "first edge" "posts" chain.Ent_ocaml.chain_first.edge;
  Alcotest.(check int) "rest length" 1 (List.length chain.chain_rest);
  match chain.chain_rest with
  | [ step ] ->
      Alcotest.(check string) "next edge" "tags" step.chain_edge;
      Alcotest.(check (option string))
        "next alias" (Some "labels") step.chain_edge_alias
  | _ -> Alcotest.fail "unexpected chain rest"

let test_transaction_hooks () =
  let events = ref [] in
  let transaction ?options:_ () f = f () in
  let commit_hook =
    Ent_ocaml.Transaction.after_commit (fun () ->
        events := "commit" :: !events;
        Ok ())
  in
  (match Ent_ocaml.Transaction.run [ commit_hook ] transaction () (fun () -> Ok 1) with
  | Ok 1 -> Alcotest.(check (list string)) "commit hook" [ "commit" ] !events
  | Ok _ -> Alcotest.fail "unexpected transaction result"
  | Error error -> Alcotest.fail (Ent_ocaml.error_to_string error));
  events := [];
  let rollback_hook =
    Ent_ocaml.Transaction.after_rollback (fun () error ->
        events := Ent_ocaml.error_to_string error :: "rollback" :: !events;
        Ok ())
  in
  (match
     Ent_ocaml.Transaction.run [ rollback_hook ] transaction () (fun () ->
         Error (`Bad_query "rollback"))
   with
  | Error (`Bad_query "rollback") ->
      Alcotest.(check (list string))
        "rollback hook" [ "bad query: rollback"; "rollback" ] !events
  | Ok _ -> Alcotest.fail "expected transaction error"
  | Error error -> Alcotest.fail (Ent_ocaml.error_to_string error));
  let failing_hook =
    Ent_ocaml.Transaction.after_commit (fun () ->
        Error (`Bad_query "commit hook failed"))
  in
  match Ent_ocaml.Transaction.run [ failing_hook ] transaction () (fun () -> Ok ()) with
  | Error (`Bad_query "commit hook failed") -> ()
  | Ok _ -> Alcotest.fail "expected commit hook error"
  | Error error -> Alcotest.fail (Ent_ocaml.error_to_string error)

let test_transaction_options () =
  let seen = ref None in
  let transaction ?options () f =
    seen := options;
    f ()
  in
  let write_concern =
    Ent_ocaml.Transaction.write_concern ~w:Ent_ocaml.Write_majority
      ~journal:true ~wtimeout_ms:250 ()
  in
  let options =
    Ent_ocaml.Transaction.options ~max_commit_time_ms:50
      ~read_concern:Ent_ocaml.Read_snapshot ~write_concern ()
  in
  match Ent_ocaml.Transaction.run ~options [] transaction () (fun () -> Ok ()) with
  | Ok () ->
      Alcotest.(check (option int))
        "max commit time"
        (Some 50)
        (Option.bind !seen (fun options -> options.max_commit_time_ms));
      Alcotest.(check bool)
        "read concern"
        true
        (Option.bind !seen (fun options -> options.read_concern)
        = Some Ent_ocaml.Read_snapshot);
      Alcotest.(check bool)
        "write concern"
        true
        (Option.bind !seen (fun options -> options.write_concern)
        = Some write_concern)
  | Error error -> Alcotest.fail (Ent_ocaml.error_to_string error)

let test_dynamic_filter_api () =
  let open Ent_ocaml in
  let status =
    Dynamic_filter.make ~field:"body" ~value:(V_string "draft")
      Dynamic_filter.Contains
  in
  (match Dynamic_filter.predicate post_entity status with
  | Ok (Contains ("body", "draft")) -> ()
  | Ok _ -> Alcotest.fail "unexpected dynamic predicate"
  | Error error -> Alcotest.fail (error_to_string error));
  let query =
    Query.make post_entity
    |> Dynamic_filter.where
         (Dynamic_filter.make ~field:"user_id" ~value:(V_string "user_1")
            Dynamic_filter.Equal)
  in
  (match query with
  | Ok query -> Alcotest.(check int) "dynamic where" 1 (List.length query.predicates)
  | Error error -> Alcotest.fail (error_to_string error));
  (match
     Dynamic_filter.predicate post_entity
       (Dynamic_filter.make ~field:"user_id" ~value:(V_int 1) Dynamic_filter.Equal)
   with
  | Error (`Bad_query "dynamic filter value has wrong type for field: user_id") -> ()
  | Ok _ -> Alcotest.fail "expected dynamic type error"
  | Error error -> Alcotest.fail (error_to_string error));
  (match
     Dynamic_filter.predicate post_entity
       (Dynamic_filter.make ~field:"missing" ~value:(V_string "x") Dynamic_filter.Equal)
   with
  | Error (`Bad_query "dynamic filter field not found on Post: missing") -> ()
  | Ok _ -> Alcotest.fail "expected dynamic field error"
  | Error error -> Alcotest.fail (error_to_string error));
  (match
     Dynamic_filter.predicate post_entity
       (Dynamic_filter.make ~field:"published_at_ms" Dynamic_filter.Is_null)
   with
  | Ok (Is_nil "published_at_ms") -> ()
  | Ok _ -> Alcotest.fail "unexpected nil predicate"
  | Error error -> Alcotest.fail (error_to_string error))

let test_entql_api () =
  let open Ent_ocaml in
  (match Entql.predicate post_entity {|body contains "draft"|} with
  | Ok (Contains ("body", "draft")) -> ()
  | Ok _ -> Alcotest.fail "unexpected entql contains predicate"
  | Error error -> Alcotest.fail (error_to_string error));
  (match Entql.predicate post_entity {|user_id in ["user_1", "user_2"]|} with
  | Ok (In ("user_id", [ V_string "user_1"; V_string "user_2" ])) -> ()
  | Ok _ -> Alcotest.fail "unexpected entql in predicate"
  | Error error -> Alcotest.fail (error_to_string error));
  (match
     Entql.where {|user_id == "user_1" && body != "draft"|}
       (Query.make post_entity)
   with
  | Ok
      {
        predicates =
          [ And [ Eq ("user_id", V_string "user_1"); Neq ("body", V_string "draft") ] ];
        _;
      } ->
      ()
  | Ok query ->
      Alcotest.failf "unexpected entql query predicates: %d"
        (List.length query.predicates)
  | Error error -> Alcotest.fail (error_to_string error));
  (match Entql.predicate post_entity {|published_at_ms > 10|} with
  | Ok (Gt ("published_at_ms", V_int64 10L)) -> ()
  | Ok _ -> Alcotest.fail "unexpected entql comparison predicate"
  | Error error -> Alcotest.fail (error_to_string error));
  (match Entql.predicate post_entity {|published_at_ms is_null|} with
  | Ok (Is_nil "published_at_ms") -> ()
  | Ok _ -> Alcotest.fail "unexpected entql null predicate"
  | Error error -> Alcotest.fail (error_to_string error));
  (match
     Entql.predicate post_entity
       {|user_id == "user_1" || (body contains "draft" && !published_at_ms is_null)|}
   with
  | Ok
      (Or
        [
          Eq ("user_id", V_string "user_1");
          And [ Contains ("body", "draft"); Not (Is_nil "published_at_ms") ];
        ]) ->
      ()
  | Ok _ -> Alcotest.fail "unexpected entql boolean predicate"
  | Error error -> Alcotest.fail (error_to_string error));
  (match Entql.predicate post_entity {|not (body contains "draft")|} with
  | Ok (Not (Contains ("body", "draft"))) -> ()
  | Ok _ -> Alcotest.fail "unexpected entql not predicate"
  | Error error -> Alcotest.fail (error_to_string error));
  (match Entql.predicate post_entity {|user.id == "user_1"|} with
  | Ok (Has_edge_with ("user", [ Eq ("id", V_string "user_1") ])) -> ()
  | Ok _ -> Alcotest.fail "unexpected entql edge id predicate"
  | Error error -> Alcotest.fail (error_to_string error));
  (match Entql.predicate post_entity {|user.id in ["user_1", "user_2"]|} with
  | Ok
      (Has_edge_with
        ("user", [ In ("id", [ V_string "user_1"; V_string "user_2" ]) ])) ->
      ()
  | Ok _ -> Alcotest.fail "unexpected entql edge id list predicate"
  | Error error -> Alcotest.fail (error_to_string error));
  (match
     Entql.predicate ~targets:[ user_entity ] post_entity
       {|user.username == "alice"|}
   with
  | Ok
      (Has_edge_with_target
        {
          edge = "user";
          target = { name = "User"; _ };
          predicates = [ Eq ("username", V_string "alice") ];
        }) ->
      ()
  | Ok _ -> Alcotest.fail "unexpected entql edge target-field predicate"
  | Error error -> Alcotest.fail (error_to_string error));
  (match
     Entql.predicate ~targets:[ user_entity; org_entity ] post_entity
       {|user.org.id == "org_1"|}
   with
  | Ok
      (Has_edge_with_target
        {
          edge = "user";
          target = { name = "User"; _ };
          predicates = [ Has_edge_with ("org", [ Eq ("id", V_string "org_1") ]) ];
        }) ->
      ()
  | Ok _ -> Alcotest.fail "unexpected entql nested edge id predicate"
  | Error error -> Alcotest.fail (error_to_string error));
  (match
     Entql.predicate ~targets:[ user_entity; org_entity ] post_entity
       {|user.org.slug == "engineering"|}
   with
  | Ok
      (Has_edge_with_target
        {
          edge = "user";
          target = { name = "User"; _ };
          predicates =
            [
              Has_edge_with_target
                {
                  edge = "org";
                  target = { name = "Org"; _ };
                  predicates = [ Eq ("slug", V_string "engineering") ];
                };
            ];
        }) ->
      ()
  | Ok _ -> Alcotest.fail "unexpected entql nested edge target predicate"
  | Error error -> Alcotest.fail (error_to_string error));
  (match Entql.predicate post_entity {|user.username == "alice"|} with
  | Error
      (`Bad_query
        "entql: edge target entity not registered for user: User") ->
      ()
  | Ok _ -> Alcotest.fail "expected entql target registry error"
  | Error error -> Alcotest.fail (error_to_string error));
  (match Entql.predicate post_entity {|metadata.flags.pinned == true|} with
  | Ok (Json_eq ("metadata", [ "flags"; "pinned" ], V_bool true)) -> ()
  | Ok _ -> Alcotest.fail "unexpected entql json bool predicate"
  | Error error -> Alcotest.fail (error_to_string error));
  (match Entql.predicate post_entity {|metadata.priority >= 10|} with
  | Ok (Json_gte ("metadata", [ "priority" ], V_int64 10L)) -> ()
  | Ok _ -> Alcotest.fail "unexpected entql json comparison predicate"
  | Error error -> Alcotest.fail (error_to_string error));
  (match
     Entql.predicate post_entity
       {|metadata.author.id in ["alice", "bob"]|}
   with
  | Ok
      (Json_in
        ("metadata", [ "author"; "id" ], [ V_string "alice"; V_string "bob" ])) ->
      ()
  | Ok _ -> Alcotest.fail "unexpected entql json list predicate"
  | Error error -> Alcotest.fail (error_to_string error));
  (match Entql.predicate post_entity {|metadata.deleted_at is_null|} with
  | Ok (Json_is_nil ("metadata", [ "deleted_at" ])) -> ()
  | Ok _ -> Alcotest.fail "unexpected entql json null predicate"
  | Error error -> Alcotest.fail (error_to_string error));
  (match Entql.predicate post_entity {|published_at_ms == "soon"|} with
  | Error (`Bad_query "entql: expected int64 value: \"soon\"") -> ()
  | Ok _ -> Alcotest.fail "expected entql type error"
  | Error error -> Alcotest.fail (error_to_string error))

let test_schema_snapshot () =
  match Ent_ocaml.Schema_snapshot.entity post_entity with
  | Ent_ocaml.V_doc fields ->
      Alcotest.(check (option string))
        "name" (Some "Post")
        (Option.map
           (function Ent_ocaml.V_string value -> value | _ -> "")
           (List.assoc_opt "name" fields));
      Alcotest.(check (option int))
        "version" (Some 1)
        (Option.map
           (function Ent_ocaml.V_int value -> value | _ -> -1)
           (List.assoc_opt "version" fields));
      Alcotest.(check bool)
        "fields" true
        (match List.assoc_opt "fields" fields with
        | Some (Ent_ocaml.V_list (_ :: _)) -> true
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
  | _ -> Alcotest.fail "expected schema snapshot document"

let test_schema_manifest () =
  match Ent_ocaml.Schema_snapshot.manifest ~name:"poster" [ post_entity ] with
  | Ent_ocaml.V_doc fields ->
      Alcotest.(check (option string))
        "name" (Some "poster")
        (Option.map
           (function Ent_ocaml.V_string value -> value | _ -> "")
           (List.assoc_opt "name" fields));
      Alcotest.(check bool)
        "entities" true
        (match List.assoc_opt "entities" fields with
        | Some (Ent_ocaml.V_list [ Ent_ocaml.V_doc entity ]) ->
            List.assoc_opt "name" entity = Some (Ent_ocaml.V_string "Post")
        | _ -> false)
  | _ -> Alcotest.fail "expected schema manifest document"

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

let test_mongo_filter_planning () =
  let filter =
    match
      Ent_ocaml_mongo.filter_to_bson
        (query
           ~predicates:
             Ent_ocaml.
               [
                 Eq ("user_id", V_string "user_1");
                 Or [ Eq ("status", V_string "draft"); Is_nil "published_at" ];
               ]
           ())
    with
    | Ok filter -> filter
    | Error error -> Alcotest.fail (Ent_ocaml.error_to_string error)
  in
  let clauses = Bson.get_list (Bson.get_element "$and" filter) in
  Alcotest.(check int) "and clauses" 2 (List.length clauses)

let test_mongo_storage_key_planning () =
  let filter =
    match
      Ent_ocaml_mongo.filter_to_bson
        (query ~predicates:Ent_ocaml.[ Eq ("id", V_string "post_1") ] ())
    with
    | Ok filter -> filter
    | Error error -> Alcotest.fail (Ent_ocaml.error_to_string error)
  in
  Alcotest.(check string)
    "id storage filter" "post_1"
    (Bson.get_string (Bson.get_element "_id" filter));
  let sort =
    Ent_ocaml_mongo.sort_to_bson ~entity:post_entity
      Ent_ocaml.[ Order.field ~direction:Asc "id" ]
    |> Option.get
  in
  Alcotest.(check int32)
    "id storage sort" 1l (Bson.get_int32 (Bson.get_element "_id" sort));
  let doc =
    match
      Ent_ocaml_mongo.document_to_bson ~entity:post_entity
        Ent_ocaml.[ ("id", V_string "post_1"); ("body", V_string "body") ]
    with
    | Ok doc -> doc
    | Error error -> Alcotest.fail (Ent_ocaml.error_to_string error)
  in
  Alcotest.(check string)
    "id storage document" "post_1"
    (Bson.get_string (Bson.get_element "_id" doc))

let test_mongo_json_path_planning () =
  let filter =
    match
      Ent_ocaml_mongo.filter_to_bson
        (query
           ~predicates:
             Ent_ocaml.
               [ Json_eq ("metadata", [ "flags"; "pinned" ], V_bool true) ]
           ())
    with
    | Ok filter -> filter
    | Error error -> Alcotest.fail (Ent_ocaml.error_to_string error)
  in
  Alcotest.(check bool)
    "json bool" true
    (Bson.get_boolean (Bson.get_element "meta.flags.pinned" filter));
  let sort =
    Ent_ocaml_mongo.sort_to_bson ~entity:post_entity
      Ent_ocaml.[ Order.field ~direction:Desc "metadata.priority" ]
    |> Option.get
  in
  Alcotest.(check int32)
    "json sort" (-1l)
    (Bson.get_int32 (Bson.get_element "meta.priority" sort));
  match
    Ent_ocaml_mongo.filter_to_bson
      (query
         ~predicates:
           Ent_ocaml.[ Json_eq ("body", [ "flags" ], V_bool true) ]
         ())
  with
  | Ok _ -> Alcotest.fail "expected non-json field error"
  | Error (`Bad_query message) ->
      Alcotest.(check string)
        "message" "json predicate field is not json: body" message
  | Error error -> Alcotest.fail (Ent_ocaml.error_to_string error)

let test_mongo_has_edge_planning () =
  let filter =
    match
      Ent_ocaml_mongo.filter_to_bson
        (query ~predicates:Ent_ocaml.[ Has_edge "user" ] ())
    with
    | Ok filter -> filter
    | Error error -> Alcotest.fail (Ent_ocaml.error_to_string error)
  in
  let user_id = Bson.get_doc_element (Bson.get_element "user_id" filter) in
  Alcotest.(check bool)
    "exists" true (Bson.get_boolean (Bson.get_element "$exists" user_id));
  ignore (Bson.get_null (Bson.get_element "$ne" user_id))

let test_mongo_has_edge_with_id_planning () =
  let filter =
    match
      Ent_ocaml_mongo.filter_to_bson
        (query
           ~predicates:
             Ent_ocaml.[ Has_edge_with ("user", [ Eq ("id", V_string "user_1") ]) ]
           ())
    with
    | Ok filter -> filter
    | Error error -> Alcotest.fail (Ent_ocaml.error_to_string error)
  in
  Alcotest.(check string)
    "user id" "user_1" (Bson.get_string (Bson.get_element "user_id" filter))

let test_mongo_sort_planning () =
  let sort =
    Ent_ocaml_mongo.sort_to_bson
      Ent_ocaml.[ Order.field ~direction:Desc "created_at_ms" ]
    |> Option.get
  in
  Alcotest.(check int32)
    "descending sort" (-1l)
    (Bson.get_int32 (Bson.get_element "created_at_ms" sort))

let test_mongo_edge_order_planning () =
  let query =
    Ent_ocaml.Query.make post_entity
    |> Ent_ocaml.Query.order_by
         Ent_ocaml.
           [
             Order.edge_field ~edge:"user" ~target:user_entity
               ~direction:Asc "username";
           ]
  in
  match Ent_ocaml_mongo.projection_to_bson query with
  | Ok _ -> ()
  | Error error -> Alcotest.fail (Ent_ocaml.error_to_string error)

let test_mongo_edge_count_order_planning () =
  let query =
    Ent_ocaml.Query.make user_entity
    |> Ent_ocaml.Query.order_by
         Ent_ocaml.
           [
             Order.edge_count ~edge:"posts" ~target:post_entity
               ~direction:Desc ~as_:"post_count" ();
           ]
  in
  match Ent_ocaml_mongo.projection_to_bson query with
  | Ok (Some projection) ->
      Alcotest.(check int32)
        "edge count alias projected" 1l
        (Bson.get_int32 (Bson.get_element "post_count" projection))
  | Ok None -> Alcotest.fail "expected projection"
  | Error error -> Alcotest.fail (Ent_ocaml.error_to_string error)

let test_mongo_backend_order_planning () =
  let expression =
    Ent_ocaml.
      V_doc [ ("$multiply", V_list [ V_string "$views"; V_int64 2L ]) ]
  in
  let order =
    Ent_ocaml_mongo.Order.expression ~name:"double_views"
      ~direction:Ent_ocaml.Desc ~as_:"score" expression
  in
  Alcotest.(check string)
    "target name" "mongo.double_views"
    (Ent_ocaml.Order.field_name order);
  let query =
    Ent_ocaml.Query.make post_entity |> Ent_ocaml.Query.order_by [ order ]
  in
  match Ent_ocaml_mongo.projection_to_bson query with
  | Ok (Some projection) ->
      Alcotest.(check int32)
        "backend order alias projected" 1l
        (Bson.get_int32 (Bson.get_element "score" projection))
  | Ok None -> Alcotest.fail "expected projection"
  | Error error -> Alcotest.fail (Ent_ocaml.error_to_string error)

let test_mongo_transaction_options_planning () =
  let write_concern =
    Ent_ocaml.Transaction.write_concern ~w:(Ent_ocaml.Write_nodes 2)
      ~journal:true ~wtimeout_ms:125 ()
  in
  let options =
    Ent_ocaml.Transaction.options ~max_commit_time_ms:50 ~write_concern ()
  in
  let command =
    Ent_ocaml_mongo.transaction_command_to_bson "commitTransaction"
      (Some options)
  in
  Alcotest.(check int64)
    "max time" 50L
    (Bson.get_int64 (Bson.get_element "maxTimeMS" command));
  let concern =
    Bson.get_doc_element (Bson.get_element "writeConcern" command)
  in
  Alcotest.(check int32)
    "write nodes" 2l (Bson.get_int32 (Bson.get_element "w" concern));
  Alcotest.(check bool)
    "write journal" true
    (Bson.get_boolean (Bson.get_element "j" concern));
  Alcotest.(check int32)
    "write timeout" 125l
    (Bson.get_int32 (Bson.get_element "wtimeout" concern))

let test_mongo_projection_planning () =
  let projection =
    match
      Ent_ocaml_mongo.projection_to_bson
        (query ~select:[ "id" ]
           ~orders:
             Ent_ocaml.
               [
                 Order.field ~as_:"ordered_body" ~direction:Desc "body";
               ]
           ())
    with
    | Ok (Some projection) -> projection
    | Ok None -> Alcotest.fail "expected projection"
    | Error error -> Alcotest.fail (Ent_ocaml.error_to_string error)
  in
  Alcotest.(check int32)
    "id storage projection" 1l
    (Bson.get_int32 (Bson.get_element "_id" projection));
  Alcotest.(check int32)
    "order value projection" 1l
    (Bson.get_int32 (Bson.get_element "body" projection))

let test_mongo_projection_missing_field () =
  match Ent_ocaml_mongo.projection_to_bson (query ~select:[ "missing" ] ()) with
  | Ok _ -> Alcotest.fail "expected missing projection field error"
  | Error (`Bad_schema message) ->
      Alcotest.(check string) "message" "field not found: missing" message
  | Error error -> Alcotest.fail (Ent_ocaml.error_to_string error)

let test_mongo_aggregate_planning () =
  let pipeline =
    match
      Ent_ocaml_mongo.aggregate_pipeline_to_bson
        (Ent_ocaml.Aggregate.max "id"
           (query
              ~predicates:Ent_ocaml.[ Eq ("user_id", V_string "user_1") ]
              ()))
    with
    | Ok pipeline -> pipeline
    | Error error -> Alcotest.fail (Ent_ocaml.error_to_string error)
  in
  Alcotest.(check int) "pipeline stages" 2 (List.length pipeline);
  let group_stage = List.nth pipeline 1 in
  let group = Bson.get_doc_element (Bson.get_element "$group" group_stage) in
  let value = Bson.get_doc_element (Bson.get_element "value" group) in
  Alcotest.(check string)
    "max storage key" "$_id"
    (Bson.get_string (Bson.get_element "$max" value))

let test_mongo_aggregate_scan_planning () =
  let scan =
    Ent_ocaml.Aggregate.scan
      Ent_ocaml.Aggregate.
        [ count_as "posts"; sum_as "views" "id" ]
      (query ~predicates:Ent_ocaml.[ Eq ("user_id", V_string "user_1") ] ())
  in
  let pipeline =
    match Ent_ocaml_mongo.aggregate_scan_pipeline_to_bson scan with
    | Ok pipeline -> pipeline
    | Error error -> Alcotest.fail (Ent_ocaml.error_to_string error)
  in
  Alcotest.(check int) "scan pipeline stages" 2 (List.length pipeline);
  let group_stage = List.nth pipeline 1 in
  let group = Bson.get_doc_element (Bson.get_element "$group" group_stage) in
  let posts = Bson.get_doc_element (Bson.get_element "posts" group) in
  let views = Bson.get_doc_element (Bson.get_element "views" group) in
  Alcotest.(check int32)
    "scan count" 1l (Bson.get_int32 (Bson.get_element "$sum" posts));
  Alcotest.(check string)
    "scan storage key" "$_id"
    (Bson.get_string (Bson.get_element "$sum" views))

let test_mongo_group_planning () =
  let pipeline =
    match
      Ent_ocaml_mongo.group_pipeline_to_bson
        (Ent_ocaml.Aggregate.group_by "user_id"
           (Ent_ocaml.Aggregate.sum "id" (query ())))
    with
    | Ok pipeline -> pipeline
    | Error error -> Alcotest.fail (Ent_ocaml.error_to_string error)
  in
  Alcotest.(check int) "pipeline stages" 1 (List.length pipeline);
  let group_stage = List.hd pipeline in
  let group = Bson.get_doc_element (Bson.get_element "$group" group_stage) in
  Alcotest.(check string)
    "group storage key" "$user_id"
    (Bson.get_string (Bson.get_element "_id" group))

let test_mongo_update_planning () =
  let update =
    match
      Ent_ocaml_mongo.update_to_bson
        (mutation Ent_ocaml.Update_one
           ~set:Ent_ocaml.[ ("body", V_string "updated") ]
           ~clear:[ "published_at_ms" ]
           ~add:Ent_ocaml.[ ("revision", V_int 1) ])
    with
    | Ok update -> update
    | Error error -> Alcotest.fail (Ent_ocaml.error_to_string error)
  in
  let set = Bson.get_doc_element (Bson.get_element "$set" update) in
  let unset = Bson.get_doc_element (Bson.get_element "$unset" update) in
  let inc = Bson.get_doc_element (Bson.get_element "$inc" update) in
  Alcotest.(check string)
    "set body" "updated" (Bson.get_string (Bson.get_element "body" set));
  Alcotest.(check string)
    "unset field" "" (Bson.get_string (Bson.get_element "published_at_ms" unset));
  Alcotest.(check int64)
    "increment" 1L (Bson.get_int64 (Bson.get_element "revision" inc))

let test_mongo_upsert_planning () =
  let update =
    match
      Ent_ocaml_mongo.update_to_bson
        (mutation Ent_ocaml.Upsert_one
           ~set:Ent_ocaml.[ ("body", V_string "updated") ]
           ~on_insert:
             Ent_ocaml.
               [
                 ("id", V_string "post_1");
                 ("user_id", V_string "user_1");
               ])
    with
    | Ok update -> update
    | Error error -> Alcotest.fail (Ent_ocaml.error_to_string error)
  in
  let set = Bson.get_doc_element (Bson.get_element "$set" update) in
  let set_on_insert =
    Bson.get_doc_element (Bson.get_element "$setOnInsert" update)
  in
  Alcotest.(check string)
    "set body" "updated" (Bson.get_string (Bson.get_element "body" set));
  Alcotest.(check string)
    "insert id" "post_1"
    (Bson.get_string (Bson.get_element "_id" set_on_insert));
  Alcotest.(check string)
    "insert user" "user_1"
    (Bson.get_string (Bson.get_element "user_id" set_on_insert))

let test_mongo_document_planning () =
  let doc =
    match
      Ent_ocaml_mongo.document_to_bson
        Ent_ocaml.
          [
            ("id", V_string "post_1");
            ("media_ids", V_list [ V_string "media_1"; V_string "media_2" ]);
            ("published_at_ms", V_null);
          ]
    with
    | Ok doc -> doc
    | Error error -> Alcotest.fail (Ent_ocaml.error_to_string error)
  in
  Alcotest.(check string)
    "id" "post_1" (Bson.get_string (Bson.get_element "id" doc));
  Alcotest.(check int)
    "media count" 2
    (Bson.get_list (Bson.get_element "media_ids" doc) |> List.length);
  ignore (Bson.get_null (Bson.get_element "published_at_ms" doc))

let test_mongo_decode_documents () =
  let doc value = Bson.add_element "name" (Bson.create_string value) Bson.empty in
  let decode doc =
    try Ok (Bson.get_string (Bson.get_element "name" doc)) with
    | _ -> Error "missing name"
  in
  match Ent_ocaml_mongo.decode_documents ~decode [ doc "a"; doc "b" ] with
  | Error error -> Alcotest.fail (Ent_ocaml.error_to_string error)
  | Ok values -> Alcotest.(check (list string)) "decoded" [ "a"; "b" ] values

let test_mongo_decode_error () =
  let decode _ = Error "bad document" in
  match Ent_ocaml_mongo.decode_document ~decode Bson.empty with
  | Ok _ -> Alcotest.fail "expected decode error"
  | Error (`Decode message) ->
      Alcotest.(check string) "decode message" "bad document" message
  | Error error -> Alcotest.fail (Ent_ocaml.error_to_string error)

let test_mongo_index_storage_fields () =
  let index =
    Ent_ocaml.
      {
        name = Some "unique_posts_id";
        fields = [ "id" ];
        edges = [];
        unique = true;
        partial_filter = [];
      }
  in
  match Ent_ocaml_mongo.index_storage_fields post_entity index with
  | Error error -> Alcotest.fail (Ent_ocaml.error_to_string error)
  | Ok fields -> Alcotest.(check (list string)) "storage fields" [ "_id" ] fields

let test_mongo_compound_index_storage_fields () =
  let index =
    Ent_ocaml.
      {
        name = Some "posts_by_id_body";
        fields = [ "id"; "body" ];
        edges = [];
        unique = false;
        partial_filter = [];
      }
  in
  match Ent_ocaml_mongo.index_storage_fields post_entity index with
  | Error error -> Alcotest.fail (Ent_ocaml.error_to_string error)
  | Ok fields ->
      Alcotest.(check (list string)) "storage fields" [ "_id"; "body" ] fields

let test_mongo_partial_index_bson () =
  let index =
    Ent_ocaml.
      {
        name = Some "published_body";
        fields = [ "body" ];
        edges = [];
        unique = false;
        partial_filter = [ Not_nil "published_at_ms" ];
      }
  in
  match Ent_ocaml_mongo.index_to_bson post_entity index with
  | Error error -> Alcotest.fail (Ent_ocaml.error_to_string error)
  | Ok bson ->
      let partial =
        Bson.get_doc_element (Bson.get_element "partialFilterExpression" bson)
      in
      let published_at =
        Bson.get_doc_element (Bson.get_element "published_at_ms" partial)
      in
      ignore (Bson.get_null (Bson.get_element "$ne" published_at));
      Alcotest.(check string)
        "name" "published_body" (Bson.get_string (Bson.get_element "name" bson))

let test_mongo_collection_validator_bson () =
  let validator = Ent_ocaml_mongo.collection_validator_to_bson post_entity in
  let schema =
    Bson.get_doc_element (Bson.get_element "$jsonSchema" validator)
  in
  let required =
    Bson.get_list (Bson.get_element "required" schema) |> List.map Bson.get_string
  in
  Alcotest.(check (list string))
    "required storage keys" [ "_id"; "user_id" ] required;
  let properties =
    Bson.get_doc_element (Bson.get_element "properties" schema)
  in
  let id_schema =
    Bson.get_doc_element (Bson.get_element "_id" properties)
  in
  Alcotest.(check string)
    "id type" "string" (Bson.get_string (Bson.get_element "bsonType" id_schema));
  let published_schema =
    Bson.get_doc_element (Bson.get_element "published_at_ms" properties)
  in
  let published_types =
    Bson.get_list (Bson.get_element "bsonType" published_schema)
    |> List.map Bson.get_string
  in
  Alcotest.(check (list string)) "option types" [ "null"; "long" ] published_types

let test_mongo_index_missing_field () =
  let index =
    Ent_ocaml.
      {
        name = Some "bad";
        fields = [ "missing" ];
        edges = [];
        unique = false;
        partial_filter = [];
      }
  in
  match Ent_ocaml_mongo.index_storage_fields post_entity index with
  | Ok _ -> Alcotest.fail "expected missing field error"
  | Error (`Bad_schema message) ->
      Alcotest.(check string) "message" "index field not found: missing" message
  | Error error -> Alcotest.fail (Ent_ocaml.error_to_string error)

let () =
  Alcotest.run "ent-ocaml"
    [
      ( "core",
        [
          Alcotest.test_case "error strings" `Quick test_error_to_string;
          Alcotest.test_case "validate create required" `Quick
            test_validate_create_missing_required;
          Alcotest.test_case "validate unknown field" `Quick
            test_validate_unknown_field;
          Alcotest.test_case "validate immutable update" `Quick
            test_validate_immutable_update;
          Alcotest.test_case "validate field validator" `Quick
            test_validate_field_validator;
          Alcotest.test_case "validate upsert required" `Quick
            test_validate_upsert_required_on_insert;
          Alcotest.test_case "validate upsert immutable" `Quick
            test_validate_upsert_rejects_immutable_set;
          Alcotest.test_case "validate update on_insert" `Quick
            test_validate_update_rejects_on_insert;
          Alcotest.test_case "query pipeline api" `Quick
            test_query_pipeline_api;
          Alcotest.test_case "query seek pagination" `Quick
            test_query_seek_pagination;
          Alcotest.test_case "query composite cursor pagination" `Quick
            test_query_composite_cursor_pagination;
          Alcotest.test_case "result syntax" `Quick test_result_syntax;
          Alcotest.test_case "privacy rule chain" `Quick test_privacy_rule_chain;
          Alcotest.test_case "mutation hook chain" `Quick test_mutation_hook_chain;
          Alcotest.test_case "query interceptor chain" `Quick
            test_query_interceptor_chain;
          Alcotest.test_case "edge interceptor chain" `Quick
            test_edge_interceptor_chain;
          Alcotest.test_case "edge query alias" `Quick test_edge_query_alias;
          Alcotest.test_case "edge chain builder" `Quick
            test_edge_chain_builder;
          Alcotest.test_case "transaction hooks" `Quick test_transaction_hooks;
          Alcotest.test_case "transaction options" `Quick
            test_transaction_options;
          Alcotest.test_case "dynamic filter api" `Quick test_dynamic_filter_api;
          Alcotest.test_case "entql api" `Quick test_entql_api;
          Alcotest.test_case "schema snapshot" `Quick test_schema_snapshot;
          Alcotest.test_case "schema manifest" `Quick test_schema_manifest;
        ] );
      ( "mongo",
        [
          Alcotest.test_case "eq predicate bson" `Quick test_mongo_eq_predicate;
          Alcotest.test_case "filter planning" `Quick test_mongo_filter_planning;
          Alcotest.test_case "storage key planning" `Quick
            test_mongo_storage_key_planning;
          Alcotest.test_case "json path planning" `Quick
            test_mongo_json_path_planning;
          Alcotest.test_case "has edge planning" `Quick
            test_mongo_has_edge_planning;
          Alcotest.test_case "has edge with id planning" `Quick
            test_mongo_has_edge_with_id_planning;
          Alcotest.test_case "sort planning" `Quick test_mongo_sort_planning;
          Alcotest.test_case "edge order planning" `Quick
            test_mongo_edge_order_planning;
          Alcotest.test_case "edge count order planning" `Quick
            test_mongo_edge_count_order_planning;
          Alcotest.test_case "backend order planning" `Quick
            test_mongo_backend_order_planning;
          Alcotest.test_case "transaction options planning" `Quick
            test_mongo_transaction_options_planning;
          Alcotest.test_case "projection planning" `Quick
            test_mongo_projection_planning;
          Alcotest.test_case "projection missing field" `Quick
            test_mongo_projection_missing_field;
          Alcotest.test_case "aggregate planning" `Quick
            test_mongo_aggregate_planning;
          Alcotest.test_case "aggregate scan planning" `Quick
            test_mongo_aggregate_scan_planning;
          Alcotest.test_case "group planning" `Quick test_mongo_group_planning;
          Alcotest.test_case "update planning" `Quick test_mongo_update_planning;
          Alcotest.test_case "upsert planning" `Quick test_mongo_upsert_planning;
          Alcotest.test_case "document planning" `Quick
            test_mongo_document_planning;
          Alcotest.test_case "decode documents" `Quick
            test_mongo_decode_documents;
          Alcotest.test_case "decode error" `Quick test_mongo_decode_error;
          Alcotest.test_case "index storage fields" `Quick
            test_mongo_index_storage_fields;
          Alcotest.test_case "compound index storage fields" `Quick
            test_mongo_compound_index_storage_fields;
          Alcotest.test_case "partial index bson" `Quick
            test_mongo_partial_index_bson;
          Alcotest.test_case "collection validator bson" `Quick
            test_mongo_collection_validator_bson;
          Alcotest.test_case "index missing field" `Quick
            test_mongo_index_missing_field;
        ]
      );
    ]
