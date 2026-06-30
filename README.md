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
helpers, field selector constants, and order helpers. Backends execute those
typed values with `result`-returning functions such as
`Ent_ocaml_mongo.insert_many_values`.

The first production target is Poster, whose Mongo store code should be replaced
by generated entity clients once the Mongo backend and PPX are complete.

See [docs/ent-go-parity-roadmap.md](docs/ent-go-parity-roadmap.md).
