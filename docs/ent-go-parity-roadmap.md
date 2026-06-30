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
  seek pagination, stored-FK edge predicates, stored-FK to-one and to-many
  traversal and eager loading, storage-key mapping for
  filters/sorts/mutations/indexes, and basic value translation.
- `[@@deriving ent]` generation for entity metadata, functional query helpers,
  typed field predicates, typed ordering helpers, typed `after_<field>` and
  `before_<field>` seek helpers, typed `<field>_cursor` terms plus
  `after_cursor`/`before_cursor`, create/update/delete mutation values,
  functional upsert mutation helpers with insert-only field setters, functional
  mutation pipeline helpers, aggregate and group-by constructors, record create
  helpers, create-bulk helpers, named aggregate scan constructors, field
  selector constants, create-time and update-time default values including
  result-returning default helpers, typed value decoders, typed validator
  wrappers, and matching `.mli` signatures for generated helper modules.
  Type-level `[@@ent.edges ...]`
  descriptors generate FK edge metadata, clean edge predicate aliases such as
  `Post.user (User.id_eq id)` and target-aware aliases such as
  `Post.user ~target:User.user_entity (User.username_eq name)`, bulk
  `has_<edge>_with` helpers, and `query_<edge>` traversal helpers plus
  `with_<edge>` eager-load helpers.
  Generated `load_edge_named` helpers return typed loaded-edge records carrying
  edge alias/name metadata for each loaded row, and generated
  `load_edges_named` helpers return ordered named groups for multiple edge
  queries that share the same source and target decoder shape.
  Generated `Client (Backend)` modules capture backend context for entity-local
  reads and mutations and expose `with_transaction` plus `Tx` operation modules
  through the backend transaction boundary. The Mongo backend runs transaction
  bodies with explicit logical sessions, stable transaction numbers,
  `startTransaction`, `autocommit:false`, and commit/abort commands when the
  connected deployment supports Mongo transactions. Generated clients accept
  result-returning transaction hooks through `with_transaction ~hooks`, and core
  `Ent_ocaml.Transaction` helpers run after-commit and after-rollback hooks.
  Generated clients accept typed transaction options through
  `with_transaction ~options`; the Mongo backend maps `max_commit_time_ms` to
  `commitTransaction.maxTimeMS`.
  Generated Store/Client `values` and `value` helpers decode selected Mongo
  projection fields and aliased order values to `Ent_ocaml.value` rows without
  requiring a full-record decoder.
  Generated edge-field order helpers such as `user_field_order ~target`
  support stored-FK to-one ordering by related entity fields, including aliased
  selected order values. The Mongo backend routes those queries through an
  aggregate `$lookup` sort path.
  Generated edge-count order helpers such as `posts_count_order ~target`
  support stored-FK to-many ordering by related row counts, including aliased
  selected count values. The Mongo backend routes those queries through a
  `$lookup` plus `$size` aggregate sort path.
  Generated `Store.With_policy` and `Client.With_policy` modules evaluate query
  and mutation privacy rule chains before delegating to the selected backend.
  Generated `Store.With_hooks` and `Client.With_hooks` modules wrap mutation
  execution with typed middleware.
  Generated `Store.With_interceptors` and `Client.With_interceptors` modules
  wrap read-path query execution.
  Generated `Store.With_edge_interceptors` and `Client.With_edge_interceptors`
  modules wrap traversal and eager-loading edge queries. Type-level
  `[@@ent.edge_interceptors ...]` attributes generate
  `Store.Schema_edge_interceptors` and `Client.Schema_edge_interceptors`
  modules.
  Type-level `[@@ent.query_rules ...]`, `[@@ent.mutation_rules ...]`,
  `[@@ent.mutation_hooks ...]`, and `[@@ent.query_interceptors ...]`
  attributes generate `Store.Schema_policy`, `Store.Schema_hooks`,
  `Store.Schema_interceptors`, `Client.Schema_policy`, `Client.Schema_hooks`,
  and `Client.Schema_interceptors` modules.
  Entities with a supported `id` field also generate entity-local `by_id`,
  `update_id`, and `delete_id` helpers for clean primary-key query and mutation
  paths.
  Generated dynamic filter helpers validate runtime field/operator/value terms
  against the entity descriptor and then compose through normal query pipelines.
  Generated `entql_predicate` and `where_entql` helpers parse
  metadata-validated filter strings with `&&`, `||`, parentheses, negation,
  equality, comparison, membership, string, and null operators into those same
  predicates. Stored foreign-key edge ID paths such as `user.id == "user_1"`
  compile to `Has_edge_with` predicates, and generated modules use explicit
  `target_entity` edge metadata so target-field paths such as
  `user.username == "alice"` compile to target-aware edge predicates.
  Generated JSON path predicate helpers for `[@ent.json]` fields support nested
  equality, comparison, membership, and null checks, plus nested path ordering.
  Generated schema snapshots expose stable `Ent_ocaml.value` metadata documents
  for drift/debug tooling, including `[@ent.sensitive]`,
  `[@ent.deprecated "..."]`, and `[@ent.comment "..."]` field metadata.
  Repository-wide schema manifests are supported with
  `Ent_ocaml.Schema_snapshot.manifest ~name entities`.
  Type-level `[@@ent.indexes ...]` descriptors support compound, unique, and
  typed partial-filter indexes using ordinary `Ent_ocaml.predicate` values; the
  Mongo backend maps logical fields through storage keys and emits
  `partialFilterExpression`. Mongo index drift checks compare declared index
  descriptors with live `listIndexes` output through typed
  `check_indexes`/`verify_indexes` reports. Mongo collection validators are
  generated from entity field metadata as `$jsonSchema` documents, and
  `check_collection_validators`/`verify_collection_validators` compare them with
  live `listCollections` metadata.
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
| Field selection | Generated selector constants, projection API, and selected-value helpers implemented | Mongo projections |
| Update one/by ID | Generated update-one builder and entity update helper | `updateOne` with matched-count handling |
| Update many | Generated update builder returning modified count | `updateMany` |
| Delete one/many | Generated delete builders | `deleteOne` / `deleteMany` |
| Upsert | Generated functional upsert mutation with insert-only fields | Mongo `updateOne` with `upsert=true` and `$setOnInsert` |
| Field predicates | Generated typed predicates for equality, comparison, IN, string, nil | Mongo `$eq`, `$ne`, `$gt`, `$in`, regex, `$exists`, null |
| JSON predicates | Generated nested path predicates and ordering implemented for `[@ent.json]` fields | Dotted paths and aggregation expressions |
| Edge predicates | `has_edge`, `has_edge_with`, and target-aware generated edge aliases implemented for stored-FK to-one edges; join-backed predicates pending | FK fields, join collections, or `$lookup` depending edge |
| Boolean predicates | `and_`, `or_`, `not_` combinators | `$and`, `$or`, `$nor` |
| Graph traversals | Stored-FK to-one/to-many and Mongo join-backed to-many `query_<edge>` traversal implemented, including target predicates, ordering, limits, and offsets; full graph chains pending | Additional queries or aggregation `$lookup` |
| Eager loading | Stored-FK to-one/to-many and Mongo join-backed to-many `with_<edge>` loading, target predicates/order/limit/offset, alias metadata, named loaded-edge records, and same-shape grouped named loads implemented; nested and heterogeneous multi-edge loading pending | Batch secondary queries; named loaders |
| Named edges | Generated `with_<edge> ~as_` alias metadata, single-edge named load results, and ordered same-shape multi-edge groups implemented; heterogeneous result maps pending | Map from edge name/alias to loaded rows |
| Bidirectional edge refs | Optional generated in-memory backrefs | Set after eager load, avoid cycles by default |
| Pagination | Limit/offset plus single-field and composite seek cursors implemented | `limit`, `skip`, sort, stable cursor keys |
| Ordering | Field, JSON-path, stored-FK to-one edge-field, and stored-FK to-many edge-count ordering implemented, including aliased selected order values; M2M edge-count and custom backend terms pending | sort, aggregation for edge terms |
| Aggregation | count, filtered/grouped min/max/sum/avg, and named scans implemented | aggregation pipeline |
| Hooks | Generated mutation middleware via `Store.With_hooks`, `Client.With_hooks`, `Store.Schema_hooks`, and `Client.Schema_hooks` implemented; deterministic cross-source/global registration pending | Around generated mutators |
| Interceptors | Generated query middleware via `Store.With_interceptors`, `Client.With_interceptors`, `Store.Schema_interceptors`, and `Client.Schema_interceptors` implemented; traversal/eager-load edge middleware via `Store.With_edge_interceptors`, `Client.With_edge_interceptors`, `Store.Schema_edge_interceptors`, and `Client.Schema_edge_interceptors` implemented | Around query execution and traversal construction |
| Privacy | Query/mutation rule-chain evaluation, generated policy-aware Store/Client modules, and schema `Store.Schema_policy`/`Client.Schema_policy` implemented; mixin registration pending | Evaluated before backend execution |
| Mixins | Reusable fields, edges, indexes, hooks, policies | PPX composition step |
| Field defaults | Generated `[@ent.default expr]`, `[@ent.update_default expr]`, `[@ent.default_result expr]`, and `[@ent.update_default_result expr]` | OCaml expressions evaluated in create/update APIs |
| Field validators | Generated `[@ent.validate [fn1; fn2]]` wrappers for primitive, enum, option, list, JSON, and nested custom record fields | Checked before backend mutation |
| Sensitive/deprecated/comments | Generated schema metadata implemented with `[@ent.sensitive]`, `[@ent.deprecated "..."]`, and `[@ent.comment "..."]` | Snapshot/display metadata |
| Indexes | Field, edge, compound, unique, and typed partial-filter index descriptors implemented | Mongo indexes with options and `partialFilterExpression` |
| Annotations | Backend/codegen metadata, only when needed by concrete backend features | OCaml attributes and typed metadata records |
| Transactions | Generated `with_transaction` and `Tx` clients plus session-backed Mongo transaction execution, transaction hooks, and typed commit-time budget option implemented; richer read/write concern options pending | Mongo sessions/transactions where deployment supports them |
| Schema/index checks | Mongo index ensure/drift verification and collection validator ensure/drift verification implemented | `createIndexes`, `listIndexes`, `collMod`, `listCollections` |
| Schema migrations | Out of scope for Poster; do not build migration planners/generators for this roadmap | Use explicit deployment/admin operations outside ent-ocaml |
| Global IDs | Optional globally unique ID configuration | App-generated IDs or ObjectId strategy |
| Schema views | Read-only entity descriptors and generated query modules | Mongo views/aggregation-backed collections where useful |
| Schema snapshot | PPX-generated per-entity schema snapshots and repository-wide manifests implemented | Checked-in `.ml` manifest or JSON snapshot |
| Local custom code | Hand-written modules beside generated code | Ordinary OCaml modules |
| Dynamic EntQL | Metadata-validated runtime field filters, result-returning boolean expression parser, stored-FK edge ID paths, and generated edge target-field paths implemented; richer nested cross-entity/path grammar pending | Runtime predicate AST parser/builder |
| Extension/plugin systems | Out of scope for Poster; do not add extension registration/checklist work | Prefer ordinary OCaml modules and typed backend metadata only when needed |
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
   `by_id id`, `update_id id |> set ...`, `delete_id id`, record-based
   `create_record`, and record-list `create_many`. Explicit `create_values`,
   `create_many_values`, `where_all`, and `set_all` helpers remain available
   for bulk/mechanical composition. Required-field,
   unknown-field, duplicate-field, unsafe upsert-overlap, and immutable update
   validation are implemented in the core mutation validator. Generated create
   helpers apply `[@ent.default expr]` for omitted fields and generated update
   helpers apply `[@ent.update_default expr]` unless the field is explicitly set,
   added, or cleared. Generated `create_result`, `create_values_result`,
   `create_record_result`, `create_many_result`, `update_one_result`,
   `update_result`, `update_one_where_result`, `update_where_result`, and
   `update_id_result` helpers compose `[@ent.default_result expr]` and
   `[@ent.update_default_result expr]` without exceptions. Generated
   `<entity>_of_ent_value` decoders let validator wrappers support
   `[@ent.validate [fn1; fn2]]` on primitive, enum, option, list, JSON, and
   nested custom record fields. Generated context-capturing `Client (Backend)`
   modules expose the same entity-local operations as `Store (Backend)` plus a
   `with_transaction` boundary, `Tx` operation module, result-returning
   transaction hooks for after-commit and after-rollback side effects, and typed
   transaction options for backend-supported commit-time limits.

5. Poster pilot:
   model `User`, `Session`, `Post`, `Media`, and `PublishAttempt`; replace the
   repetitive query/update/find helpers in `lib/store.ml` while preserving the
   existing `Store.S` signature.

6. Edges and traversals:
   implement O2O, O2M, M2O, M2M, same-type recursive edges, edge fields, and
   `has_edge_with` predicates. For Mongo, support both embedded FK fields and
   join collections. The stored-FK subset is implemented for `Has_edge`,
   `Has_edge_with` target-ID equality/membership predicates, and target-aware
   generated edge aliases that filter to-one edges by related fields through
   Mongo `$lookup`. Stored-FK to-one/to-many and Mongo join-backed to-many
   `query_<edge>` traversal are implemented through generated Store executors;
   full traversal chains, join-backed predicates/order terms, nested traversal
   filters, and cross-collection predicate planning remain.

7. Eager loading and named edges:
   stored-FK to-one/to-many and Mongo join-backed to-many `with_<edge>` eager
   loading are implemented through generated Store executors, and generated
   `with_<edge> ~as_` preserves named-edge alias metadata. Generated
   `load_edge_named` returns loaded-edge records carrying the edge alias/name
   for each row, and generated `load_edges_named` returns ordered named groups
   for multiple edge queries with the same decoder shape.
   Nested eager loading, heterogeneous multiple edge loads, per-edge limits,
   ordering, heterogeneous result maps, and optional bidirectional backrefs
   remain.

8. Hooks, privacy, and interceptors:
   generated `Store.With_hooks` and `Client.With_hooks` modules wrap mutation
   execution with typed middleware, and generated `Store.With_policy` and
   `Client.With_policy` modules evaluate query and mutation privacy rule chains
   before backend execution. Generated `Store.With_interceptors` and
   `Client.With_interceptors` modules wrap read-path query execution. Generated
   `Store.With_edge_interceptors` and `Client.With_edge_interceptors` modules
   wrap traversal and eager-loading edge queries. Schema attributes generate
   Store and Client `Schema_policy`, `Schema_hooks`, `Schema_interceptors`, and
   `Schema_edge_interceptors` modules. Mixin-provided rules and deterministic
   cross-source/global registration order remain.

9. Aggregation, ordering, and pagination:
   filtered and grouped count/min/max/sum/avg, named aggregate scans, and
   single-field/composite seek cursors are implemented. Generated JSON path
   ordering is implemented for `[@ent.json]` fields. Stored-FK to-one
   edge-field ordering is implemented through generated `<edge>_field_order`
   helpers and Mongo `$lookup` sort pipelines. Aliased selected order values
   are implemented for field, JSON-path, edge-field, and stored-FK to-many
   edge-count sort terms. M2M edge-count ordering and custom backend terms are
   still pending.

10. Mongo schema/index checks:
   generated index descriptors and `ensure_indexes` are implemented for field,
   compound, unique, and typed partial-filter indexes. `check_indexes` and
   `verify_indexes` compare expected keys, uniqueness, and partial filters
   against live Mongo `listIndexes` output. `ensure_collection_validators`,
   `check_collection_validators`, and `verify_collection_validators` generate
   mechanical Mongo `$jsonSchema` validators from entity field metadata and
   compare them against live `listCollections` output. Audit docs for
   production rollout remain.

11. EntQL and backend-specific metadata:
   metadata-validated runtime dynamic filters are implemented for field
   predicates, an EntQL parser handles boolean expressions with `&&`, `||`,
   parentheses, negation, equality, comparison, membership, string, null
   operators, stored-FK edge ID paths, and generated edge target-field paths,
   generated JSON path predicates are implemented for JSON fields, and
   generated per-entity schema snapshots plus repository-wide snapshot manifests
   are implemented. Richer nested cross-entity/path grammar, custom annotations
   when they directly support a backend feature, and typed backend-specific
   escape hatches remain. Migrations and extension/plugin systems are not on the
   Poster roadmap; add typed backend metadata only when a concrete backend
   feature requires it.

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
