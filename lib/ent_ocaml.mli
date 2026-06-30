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

type index = {
  name : string option;
  fields : string list;
  edges : string list;
  unique : bool;
}

type entity = {
  name : string;
  collection : string;
  fields : field list;
  edges : edge list;
  indexes : index list;
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
  | And of predicate list
  | Or of predicate list
  | Not of predicate
  | Has_edge of string
  | Has_edge_with of string * predicate list
  | Backend of string * value

type order_direction = Asc | Desc

type order = {
  field : string;
  direction : order_direction;
}

type query = {
  entity : entity;
  predicates : predicate list;
  select : string list;
  orders : order list;
  limit : int option;
  offset : int option;
}

type aggregate_op = Count | Min of string | Max of string | Sum of string | Avg of string

type aggregate = {
  query : query;
  op : aggregate_op;
}

type group_aggregate = {
  aggregate : aggregate;
  group : string;
}

type group_result = {
  group : value;
  value : value option;
}

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
  val limit : int -> query -> query
  val offset : int -> query -> query
end

module Aggregate : sig
  val count : query -> aggregate
  val min : string -> query -> aggregate
  val max : string -> query -> aggregate
  val sum : string -> query -> aggregate
  val avg : string -> query -> aggregate
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

type privacy_decision = Allow | Deny of string | Skip
type 'ctx query_rule = 'ctx -> query -> privacy_decision
type 'ctx mutation_rule = 'ctx -> mutation -> privacy_decision

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
  val group : ctx -> group_aggregate -> (group_result list, error) result
  val count : ctx -> query -> (int, error) result
  val transaction : ctx -> (tx -> ('a, error) result) -> ('a, error) result
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

  val insert_values : ctx -> mutation -> (doc, error) result

  val insert_many_values :
    ?ordered:bool -> ctx -> mutation list -> (doc list, error) result

  val update_one : ctx -> mutation -> (unit, error) result
  val update : ctx -> mutation -> (int, error) result
  val upsert_one : ctx -> mutation -> (unit, error) result
  val delete : ctx -> mutation -> (int, error) result
  val aggregate : ctx -> aggregate -> (value option, error) result
  val group : ctx -> group_aggregate -> (group_result list, error) result
  val count : ctx -> query -> (int, error) result
end
