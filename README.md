# ent-ocaml

`ent-ocaml` is an OCaml 5 entity framework inspired by Ent for Go. The repo is
one Git submodule that contains three packages:

- `ent-ocaml`: core runtime types for schemas, queries, mutations, hooks,
  privacy, transactions, and backend interfaces.
- `ent-ocaml-mongo`: MongoDB backend targeting the OCaml `mongo`/Eio driver and
  BSON codecs.
- `ent-ocaml-ppx`: PPX generator for entity-specific clients, predicates, and
  builders.

The generated API is intentionally OCaml-shaped. For example, a schema record
deriving `ent` produces a module with `query`, `create`, `create_many`,
`update_one`, `update`, `delete_one`, `delete`, field predicates, field value
helpers, field selector constants, edge predicate aliases, and order helpers.
Queries can be built as pipelines:

```ocaml
let query =
  let open Post in
  query ()
  |> where (user (User.id_eq user_id))
  |> where (status_eq "draft")
  |> select [ select_id; select_body ]
  |> order_by [ created_at_ms_order ~direction:Ent_ocaml.Desc () ]
  |> limit 20
```

Edge predicates should stay typed and entity-local. Stored-FK ID predicates can
use the compact edge alias, and target-field predicates pass the target entity
descriptor explicitly so the backend can plan the lookup:

```ocaml
let by_author_name =
  let open Post in
  query ()
  |> where (user ~target:User.user_entity (User.username_eq "alice"))
```

The same composed query value can feed mutations:

```ocaml
let mutation =
  let open Post in
  by_id post_id
  |> where (user (User.id_eq user_id))
  |> update_one_where
  |> set (body body)
```

Entities with an `id` field also get primary-key helpers for the unscoped path:

```ocaml
let query =
  let open Post in
  by_id post_id

let mutation =
  let open Post in
  update_id post_id |> set (body body)

let delete =
  let open Post in
  delete_id post_id
```

Upserts use the same shape, with insert-only fields kept separate:

```ocaml
let mutation =
  let open Post in
  by_id post.id
  |> upsert_where
  |> set (body post.body)
  |> on_insert (id post.id)
  |> on_insert (user_id post.user_id)
  |> on_insert (created_at_ms now_ms)
```

Create mutations can be built as pipelines or directly from records:

```ocaml
let mutation =
  let open Post in
  create ()
  |> set (id post.id)
  |> set (user_id post.user_id)
  |> set (body post.body)

let from_record = Post.create_record post_doc
```

Fields with fallible defaults use explicit result-returning helpers:

```ocaml
type post = {
  id : string;
  body : string;
  created_at_ms : int64 [@ent.default_result now_ms_result ()];
}
[@@deriving ent]

let mutation =
  let open Ent_ocaml.Result_syntax in
  let open Post in
  let* mutation = create_values_result [ id post.id; body post.body ] in
  Ok mutation
```

Update defaults have matching result helpers:

```ocaml
let mutation =
  let open Ent_ocaml.Result_syntax in
  let open Post in
  let* mutation = update_id_result post.id in
  Ok (mutation |> set (body body))
```

Generated entity modules also expose a backend-agnostic `Store` functor:

```ocaml
module Posts = Post.Store (Ent_ocaml_mongo)

let load_drafts ctx user_id =
  let open Post in
  query ()
  |> where (user (User.id_eq user_id))
  |> where (status_eq "draft")
  |> after_cursor
       [
         created_at_ms_cursor ~direction:Ent_ocaml.Desc last_seen_created_at_ms;
         id_cursor ~direction:Ent_ocaml.Desc last_seen_id;
       ]
  |> Posts.all ctx ~decode:post_of_bson_doc_result
```

Privacy policies wrap generated stores without changing ordinary call sites:

```ocaml
module Private_posts =
  Posts.With_policy (struct
    let query_rules = [ require_user_can_read_posts ]
    let mutation_rules = [ require_user_can_write_posts ]
  end)

let load_private ctx query =
  Private_posts.all ctx ~decode:post_of_bson_doc_result query
```

The same policy module style is available on generated clients:

```ocaml
module Private_post_client =
  Post_client.With_policy (struct
    let query_rules = [ require_user_can_read_posts ]
    let mutation_rules = [ require_user_can_write_posts ]
  end)

let load_private ctx query =
  let client = Private_post_client.make ctx in
  Private_post_client.all client ~decode:post_of_bson_doc_result query
```

Generated clients capture backend context when an app wants an Ent-style client
value instead of passing `ctx` to every operation:

```ocaml
module Post_client = Post.Client (Ent_ocaml_mongo)

let save_post ctx mutation =
  let client = Post_client.make ctx in
  Post_client.with_transaction client (fun tx ->
      Post_client.Tx.insert tx mutation)
```

Transaction hooks are ordinary result-returning values. Use them when a caller
needs auditable side effects after a successful commit or after rollback:

```ocaml
let save_post ctx mutation =
  let client = Post_client.make ctx in
  let hooks =
    [
      Ent_ocaml.Transaction.hook
        ~after_commit:(fun _ctx -> audit_commit ())
        ~after_rollback:(fun _ctx error -> audit_rollback error)
        ();
    ]
  in
  Post_client.with_transaction ~hooks client (fun tx ->
      Post_client.Tx.insert tx mutation)
```

Transaction options compose through the same call. Mongo currently uses
`max_commit_time_ms` as `maxTimeMS` on `commitTransaction`:

```ocaml
let save_with_commit_budget ctx mutation =
  let client = Post_client.make ctx in
  let options = Ent_ocaml.Transaction.options ~max_commit_time_ms:500 () in
  Post_client.with_transaction ~options client (fun tx ->
      Post_client.Tx.insert tx mutation)
```

For Mongo, `with_transaction` runs operations with one logical session and a
stable transaction number, then commits on `Ok` or aborts on `Error` when the
deployment supports Mongo transactions.

Selected values decode projected rows without a full-record decoder:

```ocaml
let post_summaries ctx =
  let open Post in
  query ()
  |> select [ select_id; select_body ]
  |> Posts.values ctx
```

Order helpers can expose the sorted value under a stable alias:

```ocaml
let recent_order_values ctx =
  let open Post in
  query ()
  |> order_by [ created_at_ms_order ~direction:Ent_ocaml.Desc ~as_:"created" () ]
  |> limit 20
  |> Posts.values ctx
```

Stored-FK to-one edges can also order by a field on the related entity. The
target entity is passed explicitly so the Mongo backend can use the target
collection and storage keys without global schema state:

```ocaml
let by_author ctx =
  let open Post in
  query ()
  |> order_by
       [
         user_field_order ~target:User.user_entity
           ~direction:Ent_ocaml.Asc ~as_:"author" "username" ();
       ]
  |> limit 20
  |> Posts.values ctx
```

Stored-FK to-many edges can order by related row count without writing raw
aggregation terms:

```ocaml
let most_active_users ctx =
  let open User in
  query ()
  |> order_by
       [
         posts_count_order ~target:Post.post_entity
           ~direction:Ent_ocaml.Desc ~as_:"post_count" ();
       ]
  |> limit 20
  |> Users.values ctx
```

Mutation hooks wrap generated stores in the same module-first style:

```ocaml
module Hooked_posts =
  Posts.With_hooks (struct
    let mutation_hooks = [ audit_post_mutations; apply_write_defaults ]
  end)

let save ctx mutation =
  Hooked_posts.insert ctx mutation
```

When code carries a generated client value, register mutation hooks on the
client module instead of threading hook lists through each call:

```ocaml
module Audited_posts =
  Post_client.With_hooks (struct
    let mutation_hooks = [ audit_post_mutations ]
  end)

let save ctx mutation =
  let client = Audited_posts.make ctx in
  Audited_posts.insert client mutation
```

Query interceptors wrap generated read paths:

```ocaml
module Scoped_posts =
  Posts.With_interceptors (struct
    let query_interceptors = [ scope_posts_to_current_user ]
  end)

let load_scoped ctx query =
  Scoped_posts.all ctx ~decode:post_of_bson_doc_result query
```

Client interceptors use the same module shape and apply to `Tx` operations too:

```ocaml
module Scoped_post_client =
  Post_client.With_interceptors (struct
    let query_interceptors = [ scope_posts_to_current_user ]
  end)

let count_scoped ctx query =
  let client = Scoped_post_client.make ctx in
  Scoped_post_client.with_transaction client (fun tx ->
      Scoped_post_client.Tx.count tx query)
```

Edge interceptors wrap traversal and eager-loading edge queries when middleware
needs the edge name, alias, target, and source query together:

```ocaml
module Scoped_edges =
  Posts.With_edge_interceptors (struct
    let edge_interceptors = [ scope_post_edges_to_current_user ]
  end)

let load_scoped_authors ctx edge_query =
  Scoped_edges.traverse ctx ~decode:user_of_bson_doc_result edge_query
```

Client edge interceptors use the same module shape and apply to `Tx`
operations:

```ocaml
module Scoped_edge_client =
  Post_client.With_edge_interceptors (struct
    let edge_interceptors = [ scope_post_edges_to_current_user ]
  end)

let load_scoped_authors ctx edge_query =
  let client = Scoped_edge_client.make ctx in
  Scoped_edge_client.with_transaction client (fun tx ->
      Scoped_edge_client.Tx.traverse tx ~decode:user_of_bson_doc_result edge_query)
```

Schemas can register policy, hook, and interceptor lists directly and expose
generated modules for them:

```ocaml
type post = {
  id : string;
  body : string;
}
[@@ent.query_rules [ require_can_read_posts ]]
[@@ent.mutation_hooks [ audit_post_mutations ]]
[@@ent.query_interceptors [ scope_posts_to_current_user ]]
[@@ent.edge_interceptors [ scope_post_edges_to_current_user ]]
[@@deriving ent]

module Posts = Post.Store (Ent_ocaml_mongo)
module Post_client = Post.Client (Ent_ocaml_mongo)

let load_scoped ctx query =
  Posts.Schema_interceptors.all ctx ~decode:post_of_bson_doc_result query

let load_scoped_edge ctx edge_query =
  Posts.Schema_edge_interceptors.traverse ctx
    ~decode:user_of_bson_doc_result edge_query

let save_audited ctx mutation =
  let client = Post_client.Schema_hooks.make ctx in
  Post_client.Schema_hooks.insert client mutation
```

Runtime dynamic filters validate against entity metadata and then become normal
typed predicates:

```ocaml
let load_filtered ctx =
  let open Ent_ocaml.Result_syntax in
  let open Post in
  let filter =
    dynamic_filter ~field:select_status
      ~value:(Ent_ocaml.V_string "draft")
      Ent_ocaml.Dynamic_filter.Equal
  in
  let* query = query () |> where_dynamic filter in
  Posts.all ctx ~decode:post_of_bson_doc_result query
```

For user-provided filter strings, use the generated EntQL helpers. They parse
metadata-validated boolean expressions into the same typed predicates and
compose through `result`. Stored foreign-key edge IDs can be referenced with an
edge path such as `user.id`; generated modules also register their edge target
descriptors so related target fields can be referenced as `user.username`:

```ocaml
type post = {
  id : string;
  user_id : string;
  body : string;
}
[@@ent.edges
  [
    {
      name = "user";
      target = "User";
      target_entity = user_entity;
      storage_key = "user_id";
      cardinality = "one";
    };
  ]]
[@@deriving ent]

let load_filtered ctx =
  let open Ent_ocaml.Result_syntax in
  let open Post in
  let* query =
    query ()
    |> where_entql
         {|user.username == "alice" && (status == "draft" || body contains "hello")|}
  in
  Posts.all ctx ~decode:post_of_bson_doc_result query
```

JSON fields can be marked with `[@ent.json]` and queried through generated
path helpers:

```ocaml
type event = {
  id : string;
  metadata : Ent_ocaml.value [@ent.json] [@ent.optional];
}
[@@ent.entity "Event"] [@@ent.collection "events"]
[@@deriving ent]

let pinned =
  let open Event in
  query ()
  |> where (metadata_path_eq [ "flags"; "pinned" ] (Ent_ocaml.V_bool true))
  |> order_by [ metadata_path_order ~direction:Ent_ocaml.Desc [ "priority" ] ]
```

Generated schema snapshots provide stable metadata for drift/debug tooling:

```ocaml
let snapshot = Post.post_schema_snapshot
```

Projects can collect entity descriptors into a stable repository manifest:

```ocaml
let schema_manifest =
  Ent_ocaml.Schema_snapshot.manifest ~name:"poster"
    [ user_entity; post_entity; media_entity ]
```

Fields can carry Ent-style metadata without changing runtime validation or
persistence behavior:

```ocaml
type post = {
  id : string [@ent.key "_id"] [@ent.unique];
  body : string
  [@ent.sensitive]
  [@ent.comment "Post body text"]
  [@ent.deprecated "use summary"];
}
[@@deriving ent]
```

Indexes are schema metadata, and partial indexes reuse normal typed predicates:

```ocaml
type post = {
  id : string [@ent.key "_id"] [@ent.unique];
  user_id : string [@ent.index "posts_by_user"];
  body : string;
  status : string;
}
[@@ent.indexes
  [
    {
      name = "published_posts_by_user";
      fields = [ "user_id"; "body" ];
      unique = false;
      partial_filter =
        (let open Ent_ocaml in
         [ Eq ("status", V_string "published") ]);
    };
  ]]
[@@deriving ent]
```

The Mongo backend creates those indexes through `ensure_indexes` and maps
logical field names through storage keys before sending `partialFilterExpression`.
Use predicates that the target MongoDB server accepts for partial indexes.
Operational checks can compare schema-declared indexes and generated collection
validators with live MongoDB state:

```ocaml
let verify_schema ctx =
  let open Ent_ocaml.Result_syntax in
  let entities = [ user_entity; post_entity ] in
  let* () = Ent_ocaml_mongo.ensure_collection_validators ctx entities in
  let* () = Ent_ocaml_mongo.verify_collection_validators ctx entities in
  let* () = Ent_ocaml_mongo.ensure_indexes ctx entities in
  Ent_ocaml_mongo.verify_indexes ctx entities
```

Collection validators are generated from entity field metadata as Mongo
`$jsonSchema` documents using storage keys and simple BSON types. Keep
application/domain validation in typed OCaml validators and domain mapping.

Aggregates compose from queries too:

```ocaml
let total_views =
  let open Post in
  query ()
  |> where (user (User.id_eq user_id))
  |> sum select_views
  |> Posts.aggregate ctx
```

Grouped aggregates keep the same shape:

```ocaml
let views_by_status =
  let open Post in
  query ()
  |> where (user (User.id_eq user_id))
  |> sum select_views
  |> group_by select_status
  |> Posts.group ctx
```

Aggregate scans return several named values from one composed query:

```ocaml
let summary =
  let open Post in
  query ()
  |> where (user (User.id_eq user_id))
  |> scan [ count_as "posts"; sum_as "views" select_views ]
  |> Posts.aggregate_scan ctx
```

Stored foreign-key edge traversals are also first-class values:

```ocaml
module Posts = Post.Store (Ent_ocaml_mongo)

let load_authors ctx =
  let open Post in
  query ()
  |> where (status_eq "draft")
  |> query_user ~target:User.user_entity
  |> Posts.traverse ctx ~decode:user_of_bson_doc_result
```

The same edge value can eager-load source rows with their to-one target:

```ocaml
let drafts_with_authors ctx =
  let open Post in
  query ()
  |> where (status_eq "draft")
  |> with_user ~as_:"author" ~target:User.user_entity
  |> Posts.load_edge ctx
       ~decode_source:post_of_bson_doc_result
       ~decode_target:user_of_bson_doc_result
```

Stored-FK to-many edges use the same generated helpers. `traverse` returns the
matching target rows, and `load_edge` returns one source/target pair per loaded
target row:

```ocaml
module Users = User.Store (Ent_ocaml_mongo)

let user_posts ctx user_id =
  let open User in
  by_id user_id
  |> query_posts ~target:Post.post_entity
  |> Users.traverse ctx ~decode:post_of_bson_doc_result

let users_with_posts ctx =
  let open User in
  query ()
  |> with_posts ~as_:"posts" ~target:Post.post_entity
  |> Users.load_edge ctx
       ~decode_source:user_of_bson_doc_result
	       ~decode_target:post_of_bson_doc_result
```

Mongo join-backed to-many edges use the same query and eager-load API. Declare
the join collection and key fields on the edge metadata:

```ocaml
type tag = {
  id : string [@ent.key "_id"];
  name : string;
}
[@@ent.entity "Tag"] [@@ent.collection "tags"]
[@@deriving ent]

type post = {
  id : string [@ent.key "_id"];
  body : string;
  status : string;
}
[@@ent.edges
  [
    {
      name = "tags";
      target = "Tag";
      target_entity = tag_entity;
      join_collection = "post_tags";
      join_source_key = "post_id";
      join_target_key = "tag_id";
      cardinality = "many";
    };
  ]]
[@@deriving ent]

let draft_tags ctx =
  let open Post in
  query ()
  |> where (status_eq "draft")
  |> with_tags ~target:Tag.tag_entity
  |> Posts.load_edge ctx
       ~decode_source:post_of_bson_doc_result
       ~decode_target:tag_of_bson_doc_result
```

The optional `~as_` label is preserved on the edge query so higher-level loaders
can distinguish several eager loads of the same edge. Use `load_edge_named`
when the result should carry that edge name with every row:

```ocaml
let named_authors ctx =
  let open Post in
  query ()
  |> with_user ~as_:"author" ~target:User.user_entity
  |> Posts.load_edge_named ctx
       ~decode_source:post_of_bson_doc_result
       ~decode_target:user_of_bson_doc_result
```

When a page needs several named loads with the same source and target decoders,
compose the edge queries and keep the grouped result order explicit:

```ocaml
let named_people ctx =
  let open Post in
  let drafts = query () |> where (status_eq "draft") in
  Posts.load_edges_named ctx
    ~decode_source:post_of_bson_doc_result
    ~decode_target:user_of_bson_doc_result
    [
      drafts |> with_user ~as_:"author" ~target:User.user_entity;
      drafts |> with_user ~as_:"editor" ~target:User.user_entity;
    ]
```

Backends execute those
typed values with `result`-returning functions such as
`Ent_ocaml_mongo.insert_many_values`. Core mutation validation catches missing
required create/upsert fields, unknown mutation fields, duplicate mutation
fields, unsafe upsert field overlap, and immutable-field updates before backend
execution. Mongo planning maps entity field names through storage keys for
filters, sorting, projections, inserts, updates, upserts, indexes, and
aggregates, so callers keep writing `id_eq value` even when the document stores
that field as `_id`. Fields annotated with
`[@ent.default expr]` are inserted by generated create helpers when omitted, and
`[@ent.update_default expr]` is inserted by generated update helpers unless the
field is explicitly set, added, or cleared. Use `[@ent.default_result expr]`
and `[@ent.update_default_result expr]` when computing a default can fail; call
the generated `_result` helpers and compose with `Ent_ocaml.Result_syntax`.
Fields annotated with
`[@ent.validate [fn1; fn2]]` run typed validator functions during core mutation
validation, including option, list, and nested record fields. Generated
`<entity>_of_ent_value` decoders power those typed wrappers and are also
available for local tooling. `Ent_ocaml.Result_syntax` provides `let*` and
`let+` for direct result composition at backend/application boundaries.

The first production target is Poster, whose Mongo store code should be replaced
by generated entity clients once the Mongo backend and PPX are complete.

See [docs/ent-go-parity-roadmap.md](docs/ent-go-parity-roadmap.md).
