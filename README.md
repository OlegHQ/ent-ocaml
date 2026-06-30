# ent-ocaml

`ent-ocaml` is an OCaml 5 entity framework inspired by Ent for Go. The repo is
one Git submodule that contains three packages:

- `ent-ocaml`: core runtime types for schemas, queries, mutations, hooks,
  privacy, transactions, and backend interfaces.
- `ent-ocaml-mongo`: MongoDB backend targeting the OCaml `mongo`/Eio driver and
  BSON codecs.
- `ent-ocaml-ppx`: PPX generator for entity-specific clients, predicates, and
  builders.

The first production target is Poster, whose Mongo store code should be replaced
by generated entity clients once the Mongo backend and PPX are complete.

See [docs/ent-go-parity-roadmap.md](docs/ent-go-parity-roadmap.md).
