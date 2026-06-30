# ent-ocaml Ent Go Parity Roadmap

This roadmap is based on the upstream Ent Go repository and docs at
`github.com/ent/ent`, especially `doc/md/code-gen.md`, `crud.mdx`,
`schema-fields.mdx`, `schema-edges.mdx`, `predicates.md`, `traversals.md`,
`eager-load.mdx`, `paging.mdx`, `aggregate.md`, `hooks.md`, `privacy.mdx`,
`interceptors.mdx`, `transactions.md`, `features.md`, `schema-indexes.md`, and
`schema-mixin.md`.

Latest audit: upstream `ent/ent` commit
`69d5d4deb19599f129166634e09d33addcf3f2cc`, read on 2026-06-30 from the
repository docs under `doc/md/`.

## Package Shape

Keep one Git submodule, `vendor/ent-ocaml`, with three opam packages:

- `ent-ocaml`: core runtime. Owns schema descriptors, query and mutation ASTs,
  errors, hooks, interceptors, privacy decisions, transactions, generated-client
  support, and backend signatures.
- `ent-ocaml-mongo`: first concrete backend. Owns Mongo query planning, CRUD,
  transactions/sessions when available, index creation, schema/index checks,
  aggregation pipelines, and result/error mapping for the `mongo` Eio driver.
- `ent-ocaml-ppx`: schema and client generator. Owns PPX attributes,
  compile-time schema validation, generated entity modules, predicate modules,
  builders, edge traversals, hook/privacy/interceptor registration, and
  expansion tests.

Poster should depend on the submodule through opam pins and keep MongoDB
required. The app should keep explicit domain-to-entity mapping so generated code
does mechanical CRUD/query work, not business validation.

## Public API Principle

Ent Go is the capability checklist, not the OCaml API shape. `ent-ocaml` should
feel like a small typed OCaml library:

- Prefer records, variants, modules, labels, and `result` values over method
  chains that copy Go builder semantics.
- Use PPX to remove mechanical entity boilerplate: descriptors, predicates,
  CRUD modules, edge traversal modules, mutation records, hook/privacy plumbing,
  and backend bindings.
- Keep user-written schema declarations compact and declarative. Repetitive
  setters, string field constants, BSON field names, and collection metadata
  should be generated.
- Preserve explicit application/domain mapping. Generated code should not hide
  validation, publishing policy, authentication, or business decisions.
- Provide composable functional helpers for filters, ordering, transactions,
  hooks, privacy, and interceptors, with backend-specific escape hatches kept
  typed and local.
- Generated query APIs should read as pipelines:
  `Entity.query () |> Entity.where (...) |> Entity.select [...] |> Entity.limit n`.
  Predicate lists remain available for bulk composition, but ordinary call sites
  should not need to construct AST-looking lists by hand.
- Generated entity modules expose `Store (Backend)` functors for execution so
  backend use stays typed, local, and discoverable: `module Posts =
  Post.Store (Ent_ocaml_mongo)`.
- Do not translate Go builder chains mechanically. A generated OCaml API should
  look like compact modules, labeled arguments, typed records/variants, small
  combinators, and `result`-returning execution functions.

## Current Slice

The initial package scaffold already provides:

- Core schema descriptors for fields, edges, indexes, predicates, ordering,
  queries, edge traversal descriptors, aggregate descriptors, mutations,
  mutation validation, field validators, errors, privacy decisions, backend
  signatures, generic query combinators, result syntax, polymorphic query
  interceptor middleware, polymorphic mutation hook middleware, privacy
  rule-chain evaluation, metadata-validated dynamic filters, and a store backend
  signature for generated entity-local executors.
- Mongo planning and CRUD execution for scalar filters, boolean predicates,
  field selection/projection, ordering, limit/offset, insert, bulk insert,
  update, upsert-one with `$setOnInsert`, delete, count, filtered and grouped
  min/max/sum/avg aggregates, named aggregate scans, single-field and composite
  seek pagination, stored-FK edge predicates, stored-FK to-one traversal and eager
  loading, storage-key mapping for filters/sorts/mutations/indexes, and basic
  value translation.
- `[@@deriving ent]` generation for entity metadata, functional query helpers,
  typed field predicates, typed ordering helpers, typed `after_<field>` and
  `before_<field>` seek helpers, typed `<field>_cursor` terms plus
  `after_cursor`/`before_cursor`, create/update/delete mutation values,
  functional upsert mutation helpers with insert-only field setters, functional
  mutation pipeline helpers, aggregate and group-by constructors, record create
  helpers, create-bulk helpers, named aggregate scan constructors, field
  selector constants, create-time and update-time default values, typed
  validator wrappers, and matching `.mli` signatures for generated helper
  modules. Type-level `[@@ent.edges ...]`
  descriptors generate FK edge metadata, clean edge predicate aliases such as
  `Post.user (User.id_eq id)`, bulk `has_<edge>_with` helpers, and
  `query_<edge>` traversal helpers plus `with_<edge>` eager-load helpers.
  Generated `Client (Backend)` modules capture backend context for entity-local
  reads and mutations and expose `with_transaction` plus `Tx` operation modules
  through the backend transaction boundary.
  Generated `Store.With_policy` modules evaluate query and mutation privacy
  rule chains before delegating to the selected backend. Generated
  `Store.With_hooks` modules wrap mutation execution with typed middleware.
  Generated `Store.With_interceptors` modules wrap read-path query execution.
  Generated dynamic filter helpers validate runtime field/operator/value terms
  against the entity descriptor and then compose through normal query pipelines.
  Generated schema snapshots expose stable `Ent_ocaml.value` metadata documents
  for drift/debug tooling.
- Poster pilot integration for `User`, `Session`, `Post`, `Media`,
  `PublishState`, and `PublishAttempt` DTO entities.

## Ent Go Capability Matrix

| Ent Go capability | ent-ocaml target | Mongo mapping |
| --- | --- | --- |
| Schema-as-code | OCaml schema declarations consumed by PPX | Entity descriptor per collection |
| Generated entity structs | Generated records/modules with abstract IDs where requested | BSON DTO codecs remain explicit or derived |
| Client and per-entity clients | Generated entity-local `Store` and context-capturing `Client` modules implemented | Client wraps Mongo client/database/session |
| Create builders | Generated create records/builders with required-field checks | `insertOne`, optional upsert later |
| Create bulk | Generated bulk create with ordered/unordered option | `insertMany` |
| Query builders | Generated typed query modules | Mongo find options and filters |
| Field selection | Generated selector constants and query projection API; decoded partial helpers pending | Mongo projections |
| Update one/by ID | Generated update-one builder and entity update helper | `updateOne` with matched-count handling |
| Update many | Generated update builder returning modified count | `updateMany` |
| Delete one/many | Generated delete builders | `deleteOne` / `deleteMany` |
| Upsert | Generated functional upsert mutation with insert-only fields | Mongo `updateOne` with `upsert=true` and `$setOnInsert` |
| Field predicates | Generated typed predicates for equality, comparison, IN, string, nil | Mongo `$eq`, `$ne`, `$gt`, `$in`, regex, `$exists`, null |
| JSON predicates | Generated nested path predicates | Dotted paths and aggregation expressions |
| Edge predicates | `has_edge`, `has_edge_with` generated by edge shape | FK fields, join collections, or `$lookup` depending edge |
| Boolean predicates | `and_`, `or_`, `not_` combinators | `$and`, `$or`, `$nor` |
| Graph traversals | Stored-FK to-one `query_<edge>` traversal implemented; full graph chains pending | Additional queries or aggregation `$lookup` |
| Eager loading | Stored-FK to-one `with_<edge>` loading implemented; nested and multi-edge loading pending | Batch secondary queries; named loaders |
| Named edges | Generated named edge storage | Map from edge name/alias to loaded rows |
| Bidirectional edge refs | Optional generated in-memory backrefs | Set after eager load, avoid cycles by default |
| Pagination | Limit/offset plus single-field and composite seek cursors implemented | `limit`, `skip`, sort, stable cursor keys |
| Ordering | Field and edge-count/edge-field ordering | sort, aggregation for edge terms |
| Aggregation | count, filtered/grouped min/max/sum/avg, and named scans implemented | aggregation pipeline |
| Hooks | Generated mutation middleware via `Store.With_hooks` implemented; schema/global registration pending | Around generated mutators |
| Interceptors | Generated query middleware via `Store.With_interceptors` implemented; traversal-specific interceptors pending | Around query execution and traversal construction |
| Privacy | Query/mutation rule-chain evaluation and generated policy-aware Stores implemented; schema/mixin registration pending | Evaluated before backend execution |
| Mixins | Reusable fields, edges, indexes, hooks, policies | PPX composition step |
| Field defaults | Generated `[@ent.default expr]` and `[@ent.update_default expr]`; error-returning default funcs pending | OCaml expressions evaluated in create/update APIs |
| Field validators | Generated `[@ent.validate [fn1; fn2]]` wrappers for primitive/enum fields | Checked before backend mutation |
| Sensitive/deprecated/comments | Schema metadata and generated output controls | Hidden from display/debug helpers |
| Indexes | Field, edge, compound, unique, partial/specialized annotations | Mongo indexes with options and partial filters |
| Annotations | Backend/codegen metadata, only when needed by concrete backend features | OCaml attributes and typed metadata records |
| Transactions | Generated `with_transaction` and `Tx` clients implemented; commit/rollback hooks and session-backed Mongo transactions pending | Mongo sessions/transactions where deployment supports them |
| Schema/index checks | Runtime schema/index verification | Index manifests and optional collection validators |
| Global IDs | Optional globally unique ID configuration | App-generated IDs or ObjectId strategy |
| Schema views | Read-only entity descriptors and generated query modules | Mongo views/aggregation-backed collections where useful |
| Schema snapshot | PPX-generated per-entity schema snapshot implemented; repository-wide manifests/check-in tooling pending | Checked-in `.ml` manifest or JSON snapshot |
| Local custom code | Hand-written modules beside generated code | Ordinary OCaml modules |
| Dynamic EntQL | Metadata-validated runtime field filters implemented; parsing/string expression language pending | Runtime predicate AST parser/builder |
| SQL-only features | Backend-specific optional capabilities | Provide Mongo-specific analogs, keep SQL names out of core |
| GraphQL/gRPC integrations | Out of core for first release | Future packages, not required for Poster |

## Milestones

1. Foundation package:
   define core schema descriptors, values, predicates, orders, query/mutation
   ASTs, errors, backend signatures, hook/interceptor/privacy types, and tests.

2. Mongo query planner:
   translate all scalar predicates, boolean predicates, limit/offset/order, and
   simple CRUD mutations to BSON/Mongo driver calls. Include duplicate-key,
   not-found, bad-document, and unavailable errors. Entity field names are
   mapped to storage keys for filters, ordering, mutation documents, indexes,
   and aggregation terms; `[@ent.key "_id"] [@ent.unique]` relies on Mongo's
   implicit `_id` index instead of generating a duplicate unique index.

3. PPX schema model:
   define the user-facing schema syntax and attributes. Generate entity
   descriptors and fail at compile time for duplicate fields, bad edge refs,
   invalid indexes, missing required edge metadata, and unsupported field types.

4. Generated CRUD API:
   generate create, create-bulk, query, update-one, update-many, delete-one, and
   delete-many builders with result-returning APIs. Do not generate exception
   shortcuts for Poster. The preferred generated API is functional:
   `create () |> set ...`, `query () |> where ... |> update_one_where |> set ...`,
   `query () |> where ... |> upsert_where |> set ... |> on_insert ...`,
   record-based `create_record`, and record-list `create_many`. Explicit
   `create_values`, `create_many_values`, `where_all`, and `set_all` helpers
   remain available for bulk/mechanical composition. Required-field,
   unknown-field, duplicate-field, unsafe upsert-overlap, and immutable update
   validation are implemented in the core mutation validator. Generated create
   helpers apply `[@ent.default expr]` for omitted fields and generated update
   helpers apply `[@ent.update_default expr]` unless the field is explicitly set
   or cleared. Generated validator wrappers support `[@ent.validate [fn1; fn2]]`
   on primitive and enum fields. Generated context-capturing `Client (Backend)`
   modules expose the same entity-local operations as `Store (Backend)` plus a
   `with_transaction` boundary and `Tx` operation module. Defaults that return
   errors, validators for custom/nested fields, commit/rollback hooks, and
   session-backed Mongo transactions are still pending.

5. Poster pilot:
   model `User`, `Session`, `Post`, `Media`, and `PublishAttempt`; replace the
   repetitive query/update/find helpers in `lib/store.ml` while preserving the
   existing `Store.S` signature.

6. Edges and traversals:
   implement O2O, O2M, M2O, M2M, same-type recursive edges, edge fields, and
   `has_edge_with` predicates. For Mongo, support both embedded FK fields and
   join collections. The stored-FK subset is implemented for `Has_edge` and
   `Has_edge_with` target-ID equality/membership predicates. Stored-FK to-one
   `query_<edge>` traversal is implemented through generated Store executors;
   full traversal chains, M2M join collections, nested traversal filters, and
   cross-collection predicate planning remain.

7. Eager loading and named edges:
   stored-FK to-one `with_<edge>` eager loading is implemented through generated
   Store executors. Nested eager loading, multiple edge loads, per-edge limits,
   ordering, named aliases, and optional bidirectional backrefs remain.

8. Hooks, privacy, and interceptors:
   generated `Store.With_hooks` modules wrap mutation execution with typed
   middleware, and generated `Store.With_policy` modules evaluate query and
   mutation privacy rule chains before backend execution. Generated
   `Store.With_interceptors` modules wrap read-path query execution.
   Schema/global hook registration, traversal-specific interceptors,
   mixin-provided rules, and deterministic registration order remain.

9. Aggregation, ordering, and pagination:
   filtered and grouped count/min/max/sum/avg, named aggregate scans, and
   single-field/composite seek cursors are implemented. Edge counts,
   edge-field ordering, selected order values, and custom backend terms are
   still pending.

10. Mongo schema/index checks:
   generate index manifests, `ensure_indexes`, drift checks, collection
   validators where useful, and audit docs for production rollout.

11. EntQL and backend-specific metadata:
   metadata-validated runtime dynamic filters are implemented for field
   predicates, and generated per-entity schema snapshots are implemented.
   String parsing/expression syntax, repository-wide snapshot manifests,
   custom annotations when they directly support a backend feature, and typed
   backend-specific escape hatches remain. Do not add generator plugins unless a
   concrete user need appears.

12. Poster cutover and e2e:
   run unit tests, PPX expansion tests, Mongo driver e2e, Poster HTTP e2e, and
   store-specific persistence checks. Remove obsolete handwritten Store Mongo
   helper code after generated code covers it.

## Non-Negotiable Design Rules

- Generated code must be ordinary readable OCaml.
- Expected failures return `result`; exceptions are for bugs/cancellation.
- Core does not depend on Mongo, BSON, Eio, Dream, or Poster.
- Backend-specific features use typed metadata and escape hatches, not stringly
  flags.
- Domain validation remains outside generated BSON/entity codecs.
- The first backend is MongoDB; no in-memory fallback.
- Poster keeps one deployable binary.
