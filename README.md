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
  Post.query ()
  |> Post.where (Post.user (User.id_eq user_id))
  |> Post.where (Post.status_eq "draft")
  |> Post.select [ Post.select_id; Post.select_body ]
  |> Post.order_by [ Post.created_at_ms_order ~direction:Ent_ocaml.Desc () ]
  |> Post.limit 20
```

Backends execute those
typed values with `result`-returning functions such as
`Ent_ocaml_mongo.insert_many_values`. Core mutation validation catches missing
required create fields, unknown mutation fields, duplicate mutation fields, and
immutable-field updates before backend execution. Fields annotated with
`[@ent.default expr]` are inserted by generated create helpers when omitted, and
`[@ent.update_default expr]` is inserted by generated update helpers unless the
field is explicitly set or cleared. Fields annotated with
`[@ent.validate [fn1; fn2]]` run typed validator functions during core mutation
validation. `Ent_ocaml.Result_syntax` provides `let*` and `let+` for direct
result composition at backend/application boundaries.

The first production target is Poster, whose Mongo store code should be replaced
by generated entity clients once the Mongo backend and PPX are complete.

See [docs/ent-go-parity-roadmap.md](docs/ent-go-parity-roadmap.md).
