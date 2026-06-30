type config = {
  database : string;
}

type ctx
type doc = Bson.t

type index_check_status =
  | Present
  | Missing
  | Mismatched of string list

type index_check = {
  entity : string;
  collection : string;
  name : string;
  status : index_check_status;
}

type collection_validator_check_status =
  | Validator_present
  | Validator_missing
  | Validator_mismatched of string list

type collection_validator_check = {
  validator_entity : string;
  validator_collection : string;
  validator_status : collection_validator_check_status;
}

val create : client:Mongo_eio.direct_client -> config -> ctx

module Order : sig
  val expression :
    ?as_:string ->
    name:string ->
    direction:Ent_ocaml.order_direction ->
    Ent_ocaml.value ->
    Ent_ocaml.order
end

val filter_to_bson : Ent_ocaml.query -> (Bson.t, Ent_ocaml.error) result
val sort_to_bson : ?entity:Ent_ocaml.entity -> Ent_ocaml.order list -> Bson.t option
val projection_to_bson : Ent_ocaml.query -> (Bson.t option, Ent_ocaml.error) result
val aggregate_pipeline_to_bson :
  Ent_ocaml.aggregate -> (Bson.t list, Ent_ocaml.error) result
val aggregate_scan_pipeline_to_bson :
  Ent_ocaml.aggregate_scan -> (Bson.t list, Ent_ocaml.error) result
val group_pipeline_to_bson :
  Ent_ocaml.group_aggregate -> (Bson.t list, Ent_ocaml.error) result
val document_to_bson :
  ?entity:Ent_ocaml.entity ->
  (string * Ent_ocaml.value) list ->
  (Bson.t, Ent_ocaml.error) result
val update_to_bson : Ent_ocaml.mutation -> (Bson.t, Ent_ocaml.error) result
val predicate_to_bson :
  ?entity:Ent_ocaml.entity -> Ent_ocaml.predicate -> (Bson.t, Ent_ocaml.error) result

val index_storage_fields :
  Ent_ocaml.entity -> Ent_ocaml.index -> (string list, Ent_ocaml.error) result

val index_to_bson :
  Ent_ocaml.entity -> Ent_ocaml.index -> (Bson.t, Ent_ocaml.error) result

val index_check_ok : index_check -> bool
val index_check_to_string : index_check -> string
val check_indexes : ctx -> Ent_ocaml.entity list -> (index_check list, Ent_ocaml.error) result
val verify_indexes : ctx -> Ent_ocaml.entity list -> (unit, Ent_ocaml.error) result
val collection_validator_to_bson : Ent_ocaml.entity -> Bson.t
val collection_validator_check_ok : collection_validator_check -> bool
val collection_validator_check_to_string : collection_validator_check -> string
val ensure_collection_validators : ctx -> Ent_ocaml.entity list -> (unit, Ent_ocaml.error) result
val check_collection_validators :
  ctx -> Ent_ocaml.entity list -> (collection_validator_check list, Ent_ocaml.error) result
val verify_collection_validators :
  ctx -> Ent_ocaml.entity list -> (unit, Ent_ocaml.error) result

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

val values : ctx -> Ent_ocaml.query -> (Ent_ocaml.value list, Ent_ocaml.error) result
val value : ctx -> Ent_ocaml.query -> (Ent_ocaml.value option, Ent_ocaml.error) result

val traverse_as :
  ctx ->
  Ent_ocaml.edge_query ->
  decode:(Bson.t -> ('a, string) result) ->
  ('a list, Ent_ocaml.error) result

val traverse_chain_as :
  ctx ->
  Ent_ocaml.edge_chain ->
  decode:(Bson.t -> ('a, string) result) ->
  ('a list, Ent_ocaml.error) result

val load_edge_as :
  ctx ->
  Ent_ocaml.edge_query ->
  decode_source:(Bson.t -> ('source, string) result) ->
  decode_target:(Bson.t -> ('target, string) result) ->
  (('source * 'target option) list, Ent_ocaml.error) result

val load_edge_grouped_as :
  ctx ->
  Ent_ocaml.edge_query ->
  decode_source:(Bson.t -> ('source, string) result) ->
  decode_target:(Bson.t -> ('target, string) result) ->
  (('source, 'target) Ent_ocaml.loaded_edge_targets list, Ent_ocaml.error) result

val load_edge_chain_as :
  ctx ->
  Ent_ocaml.edge_chain ->
  decode_source:(Bson.t -> ('source, string) result) ->
  decode_target:(Bson.t -> ('target, string) result) ->
  (('source * 'target option) list, Ent_ocaml.error) result

val load_edge_chain_grouped_as :
  ctx ->
  Ent_ocaml.edge_chain ->
  decode_source:(Bson.t -> ('source, string) result) ->
  decode_target:(Bson.t -> ('target, string) result) ->
  (('source, 'target) Ent_ocaml.loaded_edge_targets list, Ent_ocaml.error) result

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
val aggregate : ctx -> Ent_ocaml.aggregate -> (Ent_ocaml.value option, Ent_ocaml.error) result
val aggregate_scan :
  ctx ->
  Ent_ocaml.aggregate_scan ->
  ((string * Ent_ocaml.value option) list, Ent_ocaml.error) result
val group :
  ctx ->
  Ent_ocaml.group_aggregate ->
  (Ent_ocaml.group_result list, Ent_ocaml.error) result

val transaction_command_to_bson :
  string -> Ent_ocaml.transaction_options option -> Bson.t

val transaction :
  ?options:Ent_ocaml.transaction_options ->
  ctx ->
  (ctx -> ('a, Ent_ocaml.error) result) ->
  ('a, Ent_ocaml.error) result
