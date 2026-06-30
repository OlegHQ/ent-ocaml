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
[@@deriving ent]

module Posts = Post.Store (Ent_ocaml_mongo)

let load_scoped ctx query =
  Posts.Schema_interceptors.all ctx ~decode:post_of_bson_doc_result query
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
compose through `result`:

```ocaml
let load_filtered ctx =
  let open Ent_ocaml.Result_syntax in
  let open Post in
  let* query =
    query ()
    |> where_entql
         {|status == "draft" || (body contains "hello" && !published_at_ms is_null)|}
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
Operational checks can compare schema-declared indexes with live MongoDB state:

```ocaml
let verify_schema ctx =
  Ent_ocaml_mongo.verify_indexes ctx [ user_entity; post_entity ]
```

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

The optional `~as_` label is preserved on the edge query so higher-level loaders
can distinguish several eager loads of the same edge.

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
