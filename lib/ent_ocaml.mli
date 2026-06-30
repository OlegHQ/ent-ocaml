type field_type =
  | String
  | Int
  | Int32
  | Int64
  | Float
  | Bool
  | Time_ms
  | Uuid
  | Bytes
  | Json
  | Enum of string list
  | List of field_type
  | Option of field_type
  | Custom of string

type value =
  | V_string of string
  | V_int of int
  | V_int32 of int32
  | V_int64 of int64
  | V_float of float
  | V_bool of bool
  | V_null
  | V_list of value list
  | V_doc of (string * value) list

type field = {
  name : string;
  storage_key : string;
  typ : field_type;
  required : bool;
  unique : bool;
  immutable : bool;
  nillable : bool;
  validators : (value -> (unit, string) result) list;
  sensitive : bool;
  deprecated : string option;
  comment : string option;
}

type edge_cardinality = One | Many
type edge_direction = To | From of { ref_name : string }

type edge = {
  name : string;
  target : string;
  direction : edge_direction;
  cardinality : edge_cardinality;
  required : bool;
  storage_key : string option;
}

type predicate =
  | Eq of string * value
  | Neq of string * value
  | Gt of string * value
  | Gte of string * value
  | Lt of string * value
  | Lte of string * value
  | In of string * value list
  | Not_in of string * value list
  | Contains of string * string
  | Has_prefix of string * string
  | Has_suffix of string * string
  | Is_nil of string
  | Not_nil of string
  | Json_eq of string * string list * value
  | Json_neq of string * string list * value
  | Json_gt of string * string list * value
  | Json_gte of string * string list * value
  | Json_lt of string * string list * value
  | Json_lte of string * string list * value
  | Json_in of string * string list * value list
  | Json_not_in of string * string list * value list
  | Json_is_nil of string * string list
  | Json_not_nil of string * string list
  | And of predicate list
  | Or of predicate list
  | Not of predicate
  | Has_edge of string
  | Has_edge_with of string * predicate list
  | Has_edge_with_target of { edge : string; target : entity; predicates : predicate list }
  | Backend of string * value

and index = {
  name : string option;
  fields : string list;
  edges : string list;
  unique : bool;
  partial_filter : predicate list;
}

and entity = {
  name : string;
  collection : string;
  fields : field list;
  edges : edge list;
  indexes : index list;
}

type order_direction = Asc | Desc

type order_target =
  | Field_order of string
  | Edge_field_order of { edge : string; target : entity; field : string }
  | Edge_count_order of { edge : string; target : entity }

type order = {
  target : order_target;
  direction : order_direction;
  value_alias : string option;
}

type cursor_term = {
  field : string;
  direction : order_direction;
  value : value;
}

type query = {
  entity : entity;
  predicates : predicate list;
  select : string list;
  orders : order list;
  limit : int option;
  offset : int option;
}

type edge_query = {
  source : query;
  edge : string;
  edge_alias : string option;
  target : entity;
  target_query : query;
}

type ('source, 'target) loaded_edge = {
  loaded_edge : string;
  loaded_alias : string option;
  loaded_name : string;
  loaded_source : 'source;
  loaded_target : 'target option;
}

type ('source, 'target) loaded_edge_group = {
  loaded_group_edge : string;
  loaded_group_alias : string option;
  loaded_group_name : string;
  loaded_group_rows : ('source, 'target) loaded_edge list;
}

type aggregate_op = Count | Min of string | Max of string | Sum of string | Avg of string

type aggregate = {
  query : query;
  op : aggregate_op;
}

type aggregate_scan = {
  query : query;
  ops : (string * aggregate_op) list;
}

type group_aggregate = {
  aggregate : aggregate;
  group : string;
}

type group_result = {
  group : value;
  value : value option;
}

module Schema_snapshot : sig
  val field_type : field_type -> value
  val field : field -> value
  val edge : edge -> value
  val predicate : predicate -> value
  val index : index -> value
  val entity : entity -> value
  val entities : entity list -> value
  val manifest : name:string -> entity list -> value
end

module Query : sig
  val make :
    ?where:predicate list ->
    ?select:string list ->
    ?order:order list ->
    ?limit:int ->
    ?offset:int ->
    entity ->
    query

  val where : predicate -> query -> query
  val where_all : predicate list -> query -> query
  val select : string list -> query -> query
  val order_by : order list -> query -> query
  val order_value : string -> order -> order
  val limit : int -> query -> query
  val offset : int -> query -> query
  val after :
    field:string ->
    direction:order_direction ->
    value ->
    query ->
    query
  val before :
    field:string ->
    direction:order_direction ->
    value ->
    query ->
    query
  val after_cursor : cursor_term list -> query -> query
  val before_cursor : cursor_term list -> query -> query
end

module Order : sig
  val field : ?as_:string -> direction:order_direction -> string -> order

  val edge_field :
    ?as_:string ->
    edge:string ->
    target:entity ->
    direction:order_direction ->
    string ->
    order

  val edge_count :
    ?as_:string ->
    edge:string ->
    target:entity ->
    direction:order_direction ->
    unit ->
    order

  val target_name : order_target -> string
  val field_name : order -> string
end

module Edge_query : sig
  val make :
    ?as_:string ->
    ?target_query:query ->
    edge:string ->
    target:entity ->
    query ->
    edge_query
end

module Edge_load : sig
  val name : edge_query -> string

  val of_pair :
    edge_query ->
    'source * 'target option ->
    ('source, 'target) loaded_edge

  val of_pairs :
    edge_query ->
    ('source * 'target option) list ->
    ('source, 'target) loaded_edge list

  val group :
    edge_query ->
    ('source, 'target) loaded_edge list ->
    ('source, 'target) loaded_edge_group

  val group_of_pairs :
    edge_query ->
    ('source * 'target option) list ->
    ('source, 'target) loaded_edge_group
end

module Aggregate : sig
  val count : query -> aggregate
  val min : string -> query -> aggregate
  val max : string -> query -> aggregate
  val sum : string -> query -> aggregate
  val avg : string -> query -> aggregate
  val count_as : string -> string * aggregate_op
  val min_as : string -> string -> string * aggregate_op
  val max_as : string -> string -> string * aggregate_op
  val sum_as : string -> string -> string * aggregate_op
  val avg_as : string -> string -> string * aggregate_op
  val scan : (string * aggregate_op) list -> query -> aggregate_scan
  val group_by : string -> aggregate -> group_aggregate
end

type mutation_op = Create | Update_one | Update | Delete_one | Delete | Upsert_one

type mutation = {
  entity : entity;
  op : mutation_op;
  predicates : predicate list;
  set : (string * value) list;
  clear : string list;
  add : (string * value) list;
  on_insert : (string * value) list;
}

module Mutation : sig
  val set : string * value -> mutation -> mutation
  val set_all : (string * value) list -> mutation -> mutation
  val clear : string -> mutation -> mutation
  val add : string * value -> mutation -> mutation
  val on_insert : string * value -> mutation -> mutation
  val on_insert_all : (string * value) list -> mutation -> mutation
end

type error =
  [ `Backend of string
  | `Bad_schema of string
  | `Bad_query of string
  | `Decode of string
  | `Denied of string
  | `Not_found
  | `Not_singular
  | `Constraint of string ]

val error_to_string : error -> string
val validate_mutation : mutation -> (unit, error) result

module Result_syntax : sig
  val ( let* ) : ('a, 'e) result -> ('a -> ('b, 'e) result) -> ('b, 'e) result
  val ( let+ ) : ('a, 'e) result -> ('a -> 'b) -> ('b, 'e) result
end

module Dynamic_filter : sig
  type op =
    | Equal
    | Not_equal
    | Greater_than
    | Greater_or_equal
    | Less_than
    | Less_or_equal
    | In_list
    | Not_in_list
    | Contains
    | Has_prefix
    | Has_suffix
    | Is_null
    | Not_null

  type t = {
    field : string;
    op : op;
    value : value option;
  }

  val make : ?value:value -> field:string -> op -> t
  val predicate : entity -> t -> (predicate, error) result
  val where : t -> query -> (query, error) result
  val where_all : t list -> query -> (query, error) result
end

module Entql : sig
  val parse : entity -> string -> (Dynamic_filter.t list, error) result
  val parse_filter : entity -> string -> (Dynamic_filter.t, error) result
  val predicate : entity -> string -> (predicate, error) result
  val where : string -> query -> (query, error) result
end

type privacy_decision = Allow | Deny of string | Skip
type 'ctx query_rule = 'ctx -> query -> privacy_decision
type 'ctx mutation_rule = 'ctx -> mutation -> privacy_decision
type ('ctx, 'a) query_executor = 'ctx -> query -> ('a, error) result
type ('ctx, 'a) edge_executor = 'ctx -> edge_query -> ('a, error) result
type ('ctx, 'a) mutation_executor = 'ctx -> mutation -> ('a, error) result

type 'ctx query_interceptor = {
  wrap_query : 'a. ('ctx, 'a) query_executor -> 'ctx -> query -> ('a, error) result;
}

type 'ctx edge_interceptor = {
  wrap_edge :
    'a. ('ctx, 'a) edge_executor -> 'ctx -> edge_query -> ('a, error) result;
}

type 'ctx mutation_hook = {
  wrap_mutation :
    'a. ('ctx, 'a) mutation_executor -> 'ctx -> mutation -> ('a, error) result;
}

type 'ctx transaction_hook = {
  after_commit : 'ctx -> (unit, error) result;
  after_rollback : 'ctx -> error -> (unit, error) result;
}

type transaction_options = {
  max_commit_time_ms : int option;
}

module Privacy : sig
  val evaluate_query :
    'ctx -> 'ctx query_rule list -> query -> (unit, error) result

  val evaluate_mutation :
    'ctx -> 'ctx mutation_rule list -> mutation -> (unit, error) result

  val evaluate_mutations :
    'ctx -> 'ctx mutation_rule list -> mutation list -> (unit, error) result
end

module Interceptor : sig
  val run_query :
    'ctx query_interceptor list ->
    ('ctx, 'a) query_executor ->
    'ctx ->
    query ->
    ('a, error) result

  val run_query_value :
    'ctx query_interceptor list -> 'ctx -> query -> (query, error) result
end

module Edge_interceptor : sig
  val run_edge :
    'ctx edge_interceptor list ->
    ('ctx, 'a) edge_executor ->
    'ctx ->
    edge_query ->
    ('a, error) result

  val run_edge_value :
    'ctx edge_interceptor list ->
    'ctx ->
    edge_query ->
    (edge_query, error) result
end

module Hook : sig
  val run_mutation :
    'ctx mutation_hook list ->
    ('ctx, 'a) mutation_executor ->
    'ctx ->
    mutation ->
    ('a, error) result

  val run_mutation_value :
    'ctx mutation_hook list -> 'ctx -> mutation -> (mutation, error) result

  val run_mutations :
    'ctx mutation_hook list -> 'ctx -> mutation list -> (mutation list, error) result
end

module Transaction : sig
  val options : ?max_commit_time_ms:int -> unit -> transaction_options

  val hook :
    ?after_commit:('ctx -> (unit, error) result) ->
    ?after_rollback:('ctx -> error -> (unit, error) result) ->
    unit ->
    'ctx transaction_hook

  val after_commit : ('ctx -> (unit, error) result) -> 'ctx transaction_hook
  val after_rollback : ('ctx -> error -> (unit, error) result) -> 'ctx transaction_hook
  val run_after_commit : 'ctx transaction_hook list -> 'ctx -> (unit, error) result

  val run_after_rollback :
    'ctx transaction_hook list -> 'ctx -> error -> (unit, error) result

  val run :
    ?options:transaction_options ->
    'ctx transaction_hook list ->
    (?options:transaction_options ->
    'ctx ->
    ('tx -> ('a, error) result) ->
    ('a, error) result) ->
    'ctx ->
    ('tx -> ('a, error) result) ->
    ('a, error) result
end

module type BACKEND = sig
  type ctx
  type doc
  type tx

  val find : ctx -> query -> (doc list, error) result
  val insert : ctx -> entity -> doc -> (doc, error) result
  val insert_many : ctx -> entity -> doc list -> (doc list, error) result
  val update : ctx -> mutation -> (int, error) result
  val upsert_one : ctx -> mutation -> (unit, error) result
  val delete : ctx -> mutation -> (int, error) result
  val aggregate : ctx -> aggregate -> (value option, error) result
  val aggregate_scan :
    ctx -> aggregate_scan -> ((string * value option) list, error) result
  val group : ctx -> group_aggregate -> (group_result list, error) result
  val count : ctx -> query -> (int, error) result
  val transaction :
    ?options:transaction_options ->
    ctx ->
    (tx -> ('a, error) result) ->
    ('a, error) result
end

module type STORE_BACKEND = sig
  type ctx
  type doc

  val find_as :
    ctx ->
    query ->
    decode:(doc -> ('a, string) result) ->
    ('a list, error) result

  val find_one_as :
    ctx ->
    query ->
    decode:(doc -> ('a, string) result) ->
    ('a option, error) result

  val values : ctx -> query -> (value list, error) result
  val value : ctx -> query -> (value option, error) result

  val traverse_as :
    ctx ->
    edge_query ->
    decode:(doc -> ('a, string) result) ->
    ('a list, error) result

  val load_edge_as :
    ctx ->
    edge_query ->
    decode_source:(doc -> ('source, string) result) ->
    decode_target:(doc -> ('target, string) result) ->
    (('source * 'target option) list, error) result

  val insert_values : ctx -> mutation -> (doc, error) result

  val insert_many_values :
    ?ordered:bool -> ctx -> mutation list -> (doc list, error) result

  val update_one : ctx -> mutation -> (unit, error) result
  val update : ctx -> mutation -> (int, error) result
  val upsert_one : ctx -> mutation -> (unit, error) result
  val delete : ctx -> mutation -> (int, error) result
  val aggregate : ctx -> aggregate -> (value option, error) result
  val aggregate_scan :
    ctx -> aggregate_scan -> ((string * value option) list, error) result
  val group : ctx -> group_aggregate -> (group_result list, error) result
  val count : ctx -> query -> (int, error) result
  val transaction :
    ?options:transaction_options ->
    ctx ->
    (ctx -> ('a, error) result) ->
    ('a, error) result
end
