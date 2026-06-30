type config = {
  database : string;
}

type ctx
type doc = Bson.t

val create : client:Mongo_eio.direct_client -> config -> ctx
val filter_to_bson : Ent_ocaml.query -> (Bson.t, Ent_ocaml.error) result
val sort_to_bson : Ent_ocaml.order list -> Bson.t option
val projection_to_bson : Ent_ocaml.query -> (Bson.t option, Ent_ocaml.error) result
val document_to_bson : (string * Ent_ocaml.value) list -> (Bson.t, Ent_ocaml.error) result
val update_to_bson : Ent_ocaml.mutation -> (Bson.t, Ent_ocaml.error) result
val predicate_to_bson :
  ?entity:Ent_ocaml.entity -> Ent_ocaml.predicate -> (Bson.t, Ent_ocaml.error) result

val index_storage_fields :
  Ent_ocaml.entity -> Ent_ocaml.index -> (string list, Ent_ocaml.error) result

val decode_document :
  decode:(Bson.t -> ('a, string) result) ->
  Bson.t ->
  ('a, Ent_ocaml.error) result

val decode_documents :
  decode:(Bson.t -> ('a, string) result) ->
  Bson.t list ->
  ('a list, Ent_ocaml.error) result

val find : ctx -> Ent_ocaml.query -> (Bson.t list, Ent_ocaml.error) result
val find_one : ctx -> Ent_ocaml.query -> (Bson.t option, Ent_ocaml.error) result

val find_as :
  ctx ->
  Ent_ocaml.query ->
  decode:(Bson.t -> ('a, string) result) ->
  ('a list, Ent_ocaml.error) result

val find_one_as :
  ctx ->
  Ent_ocaml.query ->
  decode:(Bson.t -> ('a, string) result) ->
  ('a option, Ent_ocaml.error) result

val count : ctx -> Ent_ocaml.query -> (int, Ent_ocaml.error) result
val ensure_indexes : ctx -> Ent_ocaml.entity list -> (unit, Ent_ocaml.error) result
val insert : ctx -> Ent_ocaml.entity -> Bson.t -> (Bson.t, Ent_ocaml.error) result
val insert_many :
  ?ordered:bool ->
  ctx ->
  Ent_ocaml.entity ->
  Bson.t list ->
  (Bson.t list, Ent_ocaml.error) result

val insert_values : ctx -> Ent_ocaml.mutation -> (Bson.t, Ent_ocaml.error) result
val insert_many_values :
  ?ordered:bool ->
  ctx ->
  Ent_ocaml.mutation list ->
  (Bson.t list, Ent_ocaml.error) result

val update_one : ctx -> Ent_ocaml.mutation -> (unit, Ent_ocaml.error) result
val update : ctx -> Ent_ocaml.mutation -> (int, Ent_ocaml.error) result
val upsert_one : ctx -> Ent_ocaml.mutation -> (unit, Ent_ocaml.error) result
val delete : ctx -> Ent_ocaml.mutation -> (int, Ent_ocaml.error) result
