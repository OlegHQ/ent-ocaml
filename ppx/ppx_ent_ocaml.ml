open Ppxlib

let expand_entity ~ctxt:_ name =
  let loc = name.pexp_loc in
  match name.pexp_desc with
  | Pexp_constant (Pconst_string (entity_name, _, _)) ->
      Ast_builder.Default.estring ~loc entity_name
  | _ -> Location.raise_errorf ~loc "%%ent.entity expects a string literal"

let entity_extension =
  Extension.V3.declare "ent.entity" Extension.Context.expression
    Ast_pattern.(single_expr_payload __)
    expand_entity

let () =
  Driver.register_transformation "ent-ocaml-ppx"
    ~rules:[ Context_free.Rule.extension entity_extension ]
