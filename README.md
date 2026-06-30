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
  query ()
  |> where (id_eq post_id)
  |> where (user (User.id_eq user_id))
  |> update_one_where
  |> set (body body)
```

Upserts use the same shape, with insert-only fields kept separate:

```ocaml
let mutation =
  let open Post in
  query ()
  |> where (id_eq post.id)
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

Generated entity modules also expose a backend-agnostic `Store` functor:

```ocaml
module Posts = Post.Store (Ent_ocaml_mongo)

let load_drafts ctx user_id =
  let open Post in
  query ()
  |> where (user (User.id_eq user_id))
  |> where (status_eq "draft")
  |> after_created_at_ms ~direction:Ent_ocaml.Desc last_seen_created_at_ms
  |> Posts.all ctx ~decode:post_of_bson_doc_result
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
field is explicitly set or cleared. Fields annotated with
`[@ent.validate [fn1; fn2]]` run typed validator functions during core mutation
validation. `Ent_ocaml.Result_syntax` provides `let*` and `let+` for direct
result composition at backend/application boundaries.

The first production target is Poster, whose Mongo store code should be replaced
by generated entity clients once the Mongo backend and PPX are complete.

See [docs/ent-go-parity-roadmap.md](docs/ent-go-parity-roadmap.md).
