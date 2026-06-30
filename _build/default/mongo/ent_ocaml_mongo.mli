type config = {
  database : string;
}

type ctx

val create : client:'client -> config -> ctx
val predicate_to_bson : Ent_ocaml.predicate -> (Bson.t, Ent_ocaml.error) result
