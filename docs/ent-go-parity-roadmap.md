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
  transactions/sessions when available, index creation, migration checks,
  aggregation pipelines, and result/error mapping for the `mongo` Eio driver.
- `ent-ocaml-ppx`: schema and client generator. Owns PPX attributes/extensions,
  compile-time schema validation, generated entity modules, predicate modules,
  builders, edge traversals, hook/privacy/interceptor registration, and expansion
  tests.

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
- Do not translate Go builder chains mechanically. A generated OCaml API should
  look like compact modules, labeled arguments, typed records/variants, small
  combinators, and `result`-returning execution functions.

## Current Slice

The initial package scaffold already provides:

- Core schema descriptors for fields, edges, indexes, predicates, ordering,
  queries, mutations, mutation validation, errors, privacy decisions, and
  backend signatures.
- Mongo planning and CRUD execution for scalar filters, boolean predicates,
  field selection/projection, ordering, limit/offset, insert, bulk insert,
  update, delete, count, and basic value translation.
- `[@@deriving ent]` generation for entity metadata, functional query helpers,
  typed field predicates, typed ordering helpers, create/update/delete mutation
  values, create-bulk helpers, field selector constants, create-time default
  values, and matching `.mli` signatures for generated helper modules.
- Poster pilot integration for `User`, `Session`, `Post`, `Media`,
  `PublishState`, and `PublishAttempt` DTO entities.

## Ent Go Capability Matrix

| Ent Go capability | ent-ocaml target | Mongo mapping |
| --- | --- | --- |
| Schema-as-code | OCaml schema declarations consumed by PPX | Entity descriptor per collection |
| Generated entity structs | Generated records/modules with abstract IDs where requested | BSON DTO codecs remain explicit or derived |
| Client and per-entity clients | Generated `Client`, `Tx`, and entity client modules | Client wraps Mongo client/database/session |
| Create builders | Generated create records/builders with required-field checks | `insertOne`, optional upsert later |
| Create bulk | Generated bulk create with ordered/unordered option | `insertMany` |
| Query builders | Generated typed query modules | Mongo find options and filters |
| Field selection | Generated selector constants and query projection API; decoded partial helpers pending | Mongo projections |
| Update one/by ID | Generated update-one builder and entity update helper | `updateOne` with matched-count handling |
| Update many | Generated update builder returning modified count | `updateMany` |
| Delete one/many | Generated delete builders | `deleteOne` / `deleteMany` |
| Upsert | Generated upsert create option | Mongo `updateOne`/`replaceOne` with `upsert=true` |
| Field predicates | Generated typed predicates for equality, comparison, IN, string, nil | Mongo `$eq`, `$ne`, `$gt`, `$in`, regex, `$exists`, null |
| JSON predicates | Generated nested path predicates | Dotted paths and aggregation expressions |
| Edge predicates | `has_edge`, `has_edge_with` generated by edge shape | FK fields, join collections, or `$lookup` depending edge |
| Boolean predicates | `and_`, `or_`, `not_` combinators | `$and`, `$or`, `$nor` |
| Graph traversals | Generated `query_<edge>` chain | Additional queries or aggregation `$lookup` |
| Eager loading | Generated `with_<edge>` and nested loaders | Batch secondary queries; named loaders |
| Named edges | Generated named edge storage | Map from edge name/alias to loaded rows |
| Bidirectional edge refs | Optional generated in-memory backrefs | Set after eager load, avoid cycles by default |
| Pagination | Limit, offset, cursor pagination | `limit`, `skip`, sort, stable cursor keys |
| Ordering | Field and edge-count/edge-field ordering | sort, aggregation for edge terms |
| Aggregation | count, min, max, sum, avg, group by, scan | aggregation pipeline |
| Hooks | Mutation middleware, global and entity-specific | Around generated mutators |
| Interceptors | Query middleware and traversal interceptors | Around query execution and traversal construction |
| Privacy | Query/mutation rule chains with allow/deny/skip | Evaluated before backend execution |
| Mixins | Reusable fields, edges, indexes, hooks, policies | PPX composition step |
| Field defaults | Generated `[@ent.default expr]` application before insert; update defaults and error-returning default funcs pending | OCaml expressions evaluated in create API |
| Field validators | Generated validator chains for create/update values | Checked before backend mutation |
| Sensitive/deprecated/comments | Schema metadata and generated output controls | Hidden from display/debug helpers |
| Indexes | Field, edge, compound, unique, partial/specialized annotations | Mongo indexes with options and partial filters |
| Annotations | Backend/codegen metadata extension point | OCaml attributes and extensible annotation records |
| Transactions | Tx client, with-tx helper, commit/rollback hooks | Mongo sessions/transactions where deployment supports them |
| Migrations | Auto/versioned migration equivalent | Index/schema validation first; collection validators later |
| Data migrations | Versioned scripts with test helpers | Explicit migration modules using Mongo client |
| Global IDs | Optional globally unique ID configuration | App-generated IDs or ObjectId strategy |
| Schema views | Read-only entity descriptors and generated query modules | Mongo views/aggregation-backed collections where useful |
| Schema snapshot | PPX-generated schema manifest for conflict/debugging | Checked-in `.ml` manifest or JSON snapshot |
| External templates/extensions | Generator hooks and extension output | PPX extension modules/templates later |
| Dynamic EntQL | Runtime generic filters | Runtime predicate AST parser/builder |
| SQL-only features | Backend-specific optional capabilities | Provide Mongo-specific analogs, keep SQL names out of core |
| GraphQL/gRPC integrations | Out of core for first release | Future packages, not required for Poster |

## Milestones

1. Foundation package:
   define core schema descriptors, values, predicates, orders, query/mutation
   ASTs, errors, backend signatures, hook/interceptor/privacy types, and tests.

2. Mongo query planner:
   translate all scalar predicates, boolean predicates, limit/offset/order, and
   simple CRUD mutations to BSON/Mongo driver calls. Include duplicate-key,
   not-found, bad-document, and unavailable errors.

3. PPX schema model:
   define the user-facing schema syntax and attributes. Generate entity
   descriptors and fail at compile time for duplicate fields, bad edge refs,
   invalid indexes, missing required edge metadata, and unsupported field types.

4. Generated CRUD API:
   generate create, create-bulk, query, update-one, update-many, delete-one, and
   delete-many builders with result-returning APIs. Do not generate exception
   shortcuts for Poster. Create-bulk is implemented as an OCaml list-based API;
   required-field, unknown-field, duplicate-field, and immutable update
   validation are implemented in the core mutation validator. User-defined field
   validator chains are still pending. Generated create helpers apply
   `[@ent.default expr]` for omitted fields; update defaults and defaults that
   return errors are still pending.

5. Poster pilot:
   model `User`, `Session`, `Post`, `Media`, and `PublishAttempt`; replace the
   repetitive query/update/find helpers in `lib/store.ml` while preserving the
   existing `Store.S` signature.

6. Edges and traversals:
   implement O2O, O2M, M2O, M2M, same-type recursive edges, edge fields, and
   `has_edge_with` predicates. For Mongo, support both embedded FK fields and
   join collections.

7. Eager loading and named edges:
   add generated `with_<edge>` loaders, nested eager loading, per-edge filters,
   limits, ordering, named aliases, and optional bidirectional backrefs.

8. Hooks, privacy, and interceptors:
   support runtime and schema hooks, query interceptors, traversal interceptors,
   privacy rule chains, mixin-provided rules, and deterministic registration
   order.

9. Aggregation, ordering, and pagination:
   implement count/min/max/sum/avg, group-by, edge counts, edge-field ordering,
   cursor pagination, selected order values, and custom backend terms.

10. Mongo migrations and indexes:
   generate index manifests, `ensure_indexes`, drift checks, collection
   validators where useful, versioned migration files if schema-changing
   operations become necessary, and audit docs for production rollout.

11. EntQL and extension system:
   add runtime dynamic filters, schema snapshots, generator hooks, external
   template/output hooks, custom annotations, and backend-specific escape hatches.

12. Poster cutover and e2e:
   run unit tests, PPX expansion tests, Mongo driver e2e, Poster HTTP e2e, and
   store-specific persistence checks. Remove obsolete handwritten Store Mongo
   helper code after generated code covers it.

## Non-Negotiable Design Rules

- Generated code must be ordinary readable OCaml.
- Expected failures return `result`; exceptions are for bugs/cancellation.
- Core does not depend on Mongo, BSON, Eio, Dream, or Poster.
- Backend-specific features are typed extension points, not stringly flags.
- Domain validation remains outside generated BSON/entity codecs.
- The first backend is MongoDB; no in-memory fallback.
- Poster keeps one deployable binary.
