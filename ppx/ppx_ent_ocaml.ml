open Ppxlib

module A = Ast_builder.Default

let loc_of_type_decl td = td.ptype_loc
let str ~loc value = A.estring ~loc value
let bool ~loc value = A.ebool ~loc value
let evar ~loc name = A.evar ~loc name
let pvar ~loc name = A.ppat_var ~loc { loc; txt = name }

let lid_of_parts = function
  | [] -> invalid_arg "lid_of_parts"
  | part :: parts ->
      List.fold_left
        (fun acc part -> Longident.Ldot (acc, part))
        (Longident.Lident part) parts

let lid ~loc parts = { loc; txt = lid_of_parts parts }
let ident ~loc parts = A.pexp_ident ~loc (lid ~loc parts)
let constr ~loc parts = A.pexp_construct ~loc (lid ~loc parts) None
let constr_arg ~loc parts arg = A.pexp_construct ~loc (lid ~loc parts) (Some arg)
let unit_pat ~loc = A.ppat_construct ~loc (lid ~loc [ "()" ]) None

let app ~loc fn args =
  A.pexp_apply ~loc fn (List.map (fun arg -> (Nolabel, arg)) args)

let option ~loc = function
  | None -> constr ~loc [ "None" ]
  | Some expr -> A.pexp_construct ~loc (lid ~loc [ "Some" ]) (Some expr)

let list ~loc values = A.elist ~loc values

let ent_entity_attr =
  Attribute.declare "ent.entity" Attribute.Context.type_declaration
    Ast_pattern.(single_expr_payload (estring __))
    (fun name -> name)

let ent_collection_attr =
  Attribute.declare "ent.collection" Attribute.Context.type_declaration
    Ast_pattern.(single_expr_payload (estring __))
    (fun collection -> collection)

let ent_key_attr =
  Attribute.declare "ent.key" Attribute.Context.label_declaration
    Ast_pattern.(single_expr_payload (estring __))
    (fun key -> key)

let ent_unique_attr =
  Attribute.declare "ent.unique" Attribute.Context.label_declaration
    Ast_pattern.(pstr nil)
    ()

let ent_optional_attr =
  Attribute.declare "ent.optional" Attribute.Context.label_declaration
    Ast_pattern.(pstr nil)
    ()

let ent_immutable_attr =
  Attribute.declare "ent.immutable" Attribute.Context.label_declaration
    Ast_pattern.(pstr nil)
    ()

let ent_enum_attr =
  Attribute.declare "ent.enum" Attribute.Context.label_declaration
    Ast_pattern.(single_expr_payload (elist (estring __)))
    (fun values -> values)

let has_attr attr item = Attribute.get attr item |> Option.is_some

let snake_to_pascal name =
  name |> String.split_on_char '_'
  |> List.filter (fun part -> part <> "")
  |> List.map (fun part ->
         let first = String.uppercase_ascii (String.sub part 0 1) in
         if String.length part = 1 then first
         else first ^ String.sub part 1 (String.length part - 1))
  |> String.concat ""

let pluralize name =
  if String.ends_with ~suffix:"s" name then name else name ^ "s"

let type_path_parts path =
  let rec loop acc = function
    | Longident.Lident name -> name :: acc
    | Longident.Ldot (prefix, name) -> loop (name :: acc) prefix
    | Longident.Lapply _ ->
        Location.raise_errorf "ent deriving does not support applicative paths"
  in
  loop [] path

let rec field_type_expr ~loc field =
  match Attribute.get ent_enum_attr field with
  | Some values ->
      constr_arg ~loc [ "Ent_ocaml"; "Enum" ]
        (list ~loc (List.map (str ~loc) values))
  | None -> (
      match field.pld_type.ptyp_desc with
      | Ptyp_constr ({ txt = Longident.Lident "string"; _ }, []) ->
          constr ~loc [ "Ent_ocaml"; "String" ]
      | Ptyp_constr ({ txt = Longident.Lident "int"; _ }, []) ->
          constr ~loc [ "Ent_ocaml"; "Int" ]
      | Ptyp_constr ({ txt = Longident.Lident "int32"; _ }, [])
      | Ptyp_constr ({ txt = Longident.Ldot (Longident.Lident "Int32", "t"); _ }, []) ->
          constr ~loc [ "Ent_ocaml"; "Int32" ]
      | Ptyp_constr ({ txt = Longident.Lident "int64"; _ }, [])
      | Ptyp_constr ({ txt = Longident.Ldot (Longident.Lident "Int64", "t"); _ }, []) ->
          constr ~loc [ "Ent_ocaml"; "Int64" ]
      | Ptyp_constr ({ txt = Longident.Lident "float"; _ }, []) ->
          constr ~loc [ "Ent_ocaml"; "Float" ]
      | Ptyp_constr ({ txt = Longident.Lident "bool"; _ }, []) ->
          constr ~loc [ "Ent_ocaml"; "Bool" ]
      | Ptyp_constr ({ txt = Longident.Lident "bytes"; _ }, []) ->
          constr ~loc [ "Ent_ocaml"; "Bytes" ]
      | Ptyp_constr ({ txt = Longident.Lident "option"; _ }, [ inner ])
      | Ptyp_constr ({ txt = Longident.Ldot (Longident.Lident "Option", "t"); _ }, [ inner ]) ->
          constr_arg ~loc [ "Ent_ocaml"; "Option" ]
            (field_type_expr ~loc { field with pld_type = inner })
      | Ptyp_constr ({ txt = Longident.Lident "list"; _ }, [ inner ])
      | Ptyp_constr ({ txt = Longident.Ldot (Longident.Lident "List", "t"); _ }, [ inner ]) ->
          constr_arg ~loc [ "Ent_ocaml"; "List" ]
            (field_type_expr ~loc { field with pld_type = inner })
      | Ptyp_constr ({ txt = path; _ }, []) ->
          let name = String.concat "." (type_path_parts path) in
          if name = "Ptime.t" then constr ~loc [ "Ent_ocaml"; "Time_ms" ]
          else if name = "Uuidm.t" then constr ~loc [ "Ent_ocaml"; "Uuid" ]
          else if name = "Yojson.Safe.t" || name = "Yojson.t" then
            constr ~loc [ "Ent_ocaml"; "Json" ]
          else constr_arg ~loc [ "Ent_ocaml"; "Custom" ] (str ~loc name)
      | _ ->
          Location.raise_errorf ~loc:field.pld_type.ptyp_loc
            "ent deriving supports primitive fields, option fields, enum fields, or named custom types")

let is_option field =
  match field.pld_type.ptyp_desc with
  | Ptyp_constr ({ txt = Longident.Lident "option"; _ }, [ _ ])
  | Ptyp_constr ({ txt = Longident.Ldot (Longident.Lident "Option", "t"); _ }, [ _ ]) ->
      true
  | _ -> false

let rec value_constructor field =
  match Attribute.get ent_enum_attr field with
  | Some _ -> Some [ "Ent_ocaml"; "V_string" ]
  | None -> (
      match field.pld_type.ptyp_desc with
      | Ptyp_constr ({ txt = Longident.Lident "string"; _ }, []) ->
          Some [ "Ent_ocaml"; "V_string" ]
      | Ptyp_constr ({ txt = Longident.Lident "int"; _ }, []) ->
          Some [ "Ent_ocaml"; "V_int" ]
      | Ptyp_constr ({ txt = Longident.Lident "int32"; _ }, [])
      | Ptyp_constr
          ({ txt = Longident.Ldot (Longident.Lident "Int32", "t"); _ }, []) ->
          Some [ "Ent_ocaml"; "V_int32" ]
      | Ptyp_constr ({ txt = Longident.Lident "int64"; _ }, [])
      | Ptyp_constr
          ({ txt = Longident.Ldot (Longident.Lident "Int64", "t"); _ }, []) ->
          Some [ "Ent_ocaml"; "V_int64" ]
      | Ptyp_constr ({ txt = Longident.Lident "float"; _ }, []) ->
          Some [ "Ent_ocaml"; "V_float" ]
      | Ptyp_constr ({ txt = Longident.Lident "bool"; _ }, []) ->
          Some [ "Ent_ocaml"; "V_bool" ]
      | Ptyp_constr ({ txt = Longident.Lident "option"; _ }, [ inner ])
      | Ptyp_constr
          ({ txt = Longident.Ldot (Longident.Lident "Option", "t"); _ }, [ inner ])
        ->
          value_constructor { field with pld_type = inner }
      | _ -> None)

let predicate_function ~loc name constructor field_name value_path =
  let value = evar ~loc "value" in
  let body =
    constr_arg ~loc constructor
      (A.pexp_tuple ~loc
         [
           str ~loc field_name;
           constr_arg ~loc value_path value;
         ])
  in
  A.pstr_value ~loc Nonrecursive
    [
      A.value_binding ~loc
        ~pat:(pvar ~loc name)
        ~expr:(A.pexp_fun ~loc Nolabel None (pvar ~loc "value") body);
    ]

let list_predicate_function ~loc name constructor field_name value_path =
  let values = evar ~loc "values" in
  let mapper =
    A.pexp_fun ~loc Nolabel None (pvar ~loc "value")
      (constr_arg ~loc value_path (evar ~loc "value"))
  in
  let body =
    constr_arg ~loc constructor
      (A.pexp_tuple ~loc
         [
           str ~loc field_name;
           app ~loc (ident ~loc [ "List"; "map" ]) [ mapper; values ];
         ])
  in
  A.pstr_value ~loc Nonrecursive
    [
      A.value_binding ~loc
        ~pat:(pvar ~loc name)
        ~expr:(A.pexp_fun ~loc Nolabel None (pvar ~loc "values") body);
    ]

let string_predicate_function ~loc name constructor field_name =
  let value = evar ~loc "value" in
  let body =
    constr_arg ~loc constructor (A.pexp_tuple ~loc [ str ~loc field_name; value ])
  in
  A.pstr_value ~loc Nonrecursive
    [
      A.value_binding ~loc
        ~pat:(pvar ~loc name)
        ~expr:(A.pexp_fun ~loc Nolabel None (pvar ~loc "value") body);
    ]

let nullary_predicate_function ~loc name constructor field_name =
  A.pstr_value ~loc Nonrecursive
    [
      A.value_binding ~loc
        ~pat:(pvar ~loc name)
        ~expr:
          (A.pexp_fun ~loc Nolabel None (unit_pat ~loc)
             (constr_arg ~loc constructor (str ~loc field_name)));
    ]

let order_function ~loc field_name =
  let direction =
    A.pexp_match ~loc (evar ~loc "direction")
      [
        A.case ~lhs:(A.ppat_construct ~loc (lid ~loc [ "None" ]) None)
          ~guard:None ~rhs:(constr ~loc [ "Ent_ocaml"; "Asc" ]);
        A.case
          ~lhs:
            (A.ppat_construct ~loc (lid ~loc [ "Some" ])
               (Some (pvar ~loc "direction")))
          ~guard:None ~rhs:(evar ~loc "direction");
      ]
  in
  let body =
    A.pexp_record ~loc
      [
        (lid ~loc [ "Ent_ocaml"; "field" ], str ~loc field_name);
        (lid ~loc [ "Ent_ocaml"; "direction" ], direction);
      ]
      None
  in
  A.pstr_value ~loc Nonrecursive
    [
      A.value_binding ~loc
        ~pat:(pvar ~loc (field_name ^ "_order"))
        ~expr:
          (A.pexp_fun ~loc (Optional "direction") None (pvar ~loc "direction")
             (A.pexp_fun ~loc Nolabel None (unit_pat ~loc) body));
    ]

let field_helper_items field =
  let loc = field.pld_loc in
  let field_name = field.pld_name.txt in
  let order = order_function ~loc field_name in
  match value_constructor field with
  | None -> [ order ]
  | Some value_path ->
      let base =
        [
          predicate_function ~loc (field_name ^ "_eq") [ "Ent_ocaml"; "Eq" ]
            field_name value_path;
          predicate_function ~loc (field_name ^ "_neq") [ "Ent_ocaml"; "Neq" ]
            field_name value_path;
          list_predicate_function ~loc (field_name ^ "_in")
            [ "Ent_ocaml"; "In" ] field_name value_path;
          list_predicate_function ~loc (field_name ^ "_not_in")
            [ "Ent_ocaml"; "Not_in" ] field_name value_path;
          order;
        ]
      in
      let nil_helpers =
        if is_option field then
          [
            nullary_predicate_function ~loc (field_name ^ "_is_nil")
              [ "Ent_ocaml"; "Is_nil" ] field_name;
            nullary_predicate_function ~loc (field_name ^ "_not_nil")
              [ "Ent_ocaml"; "Not_nil" ] field_name;
          ]
        else []
      in
      let string_helpers =
        match value_path with
        | [ "Ent_ocaml"; "V_string" ] ->
            [
              string_predicate_function ~loc (field_name ^ "_contains")
                [ "Ent_ocaml"; "Contains" ] field_name;
              string_predicate_function ~loc (field_name ^ "_has_prefix")
                [ "Ent_ocaml"; "Has_prefix" ] field_name;
              string_predicate_function ~loc (field_name ^ "_has_suffix")
                [ "Ent_ocaml"; "Has_suffix" ] field_name;
            ]
        | _ -> []
      in
      base @ nil_helpers @ string_helpers

let field_expr field =
  let loc = field.pld_loc in
  let name = field.pld_name.txt in
  let optional = has_attr ent_optional_attr field || is_option field in
  A.pexp_record ~loc
    [
      (lid ~loc [ "Ent_ocaml"; "name" ], str ~loc name);
      ( lid ~loc [ "Ent_ocaml"; "storage_key" ],
        str ~loc (Option.value (Attribute.get ent_key_attr field) ~default:name) );
      (lid ~loc [ "Ent_ocaml"; "typ" ], field_type_expr ~loc field);
      (lid ~loc [ "Ent_ocaml"; "required" ], bool ~loc (not optional));
      (lid ~loc [ "Ent_ocaml"; "unique" ], bool ~loc (has_attr ent_unique_attr field));
      (lid ~loc [ "Ent_ocaml"; "immutable" ], bool ~loc (has_attr ent_immutable_attr field));
      (lid ~loc [ "Ent_ocaml"; "nillable" ], bool ~loc (is_option field));
    ]
    None

let ensure_record td =
  if td.ptype_params <> [] then
    Location.raise_errorf ~loc:td.ptype_loc "ent deriving does not support parameterized entity records";
  match td.ptype_kind with
  | Ptype_record fields -> fields
  | _ -> Location.raise_errorf ~loc:td.ptype_loc "ent deriving supports record entity types only"

let gen_entity td =
  let loc = loc_of_type_decl td in
  let fields = ensure_record td in
  let type_name = td.ptype_name.txt in
  let entity_name =
    Option.value (Attribute.get ent_entity_attr td) ~default:(snake_to_pascal type_name)
  in
  let collection =
    Option.value (Attribute.get ent_collection_attr td) ~default:(pluralize type_name)
  in
  let expr =
    A.pexp_record ~loc
      [
        (lid ~loc [ "Ent_ocaml"; "name" ], str ~loc entity_name);
        (lid ~loc [ "Ent_ocaml"; "collection" ], str ~loc collection);
        (lid ~loc [ "Ent_ocaml"; "fields" ], list ~loc (List.map field_expr fields));
        (lid ~loc [ "Ent_ocaml"; "edges" ], list ~loc []);
        (lid ~loc [ "Ent_ocaml"; "indexes" ], list ~loc []);
      ]
      None
  in
  A.pstr_value ~loc Nonrecursive
    [ A.value_binding ~loc ~pat:(pvar ~loc (type_name ^ "_entity")) ~expr ]

let gen_query_module td =
  let loc = loc_of_type_decl td in
  let fields = ensure_record td in
  let type_name = td.ptype_name.txt in
  let module_name = snake_to_pascal type_name in
  let query_body =
    A.pexp_record ~loc
      [
        (lid ~loc [ "Ent_ocaml"; "entity" ], evar ~loc (type_name ^ "_entity"));
        (lid ~loc [ "Ent_ocaml"; "predicates" ], evar ~loc "where");
        (lid ~loc [ "Ent_ocaml"; "orders" ], evar ~loc "order");
        (lid ~loc [ "Ent_ocaml"; "limit" ], evar ~loc "limit");
        (lid ~loc [ "Ent_ocaml"; "offset" ], evar ~loc "offset");
      ]
      None
  in
  let query =
    A.pstr_value ~loc Nonrecursive
      [
        A.value_binding ~loc ~pat:(pvar ~loc "query")
          ~expr:
            (A.pexp_fun ~loc (Optional "where") (Some (list ~loc []))
               (pvar ~loc "where")
               (A.pexp_fun ~loc (Optional "order") (Some (list ~loc []))
                  (pvar ~loc "order")
                  (A.pexp_fun ~loc (Optional "limit") None (pvar ~loc "limit")
                     (A.pexp_fun ~loc (Optional "offset") None
                        (pvar ~loc "offset")
                        (A.pexp_fun ~loc Nolabel None (unit_pat ~loc)
                           query_body)))));
      ]
  in
  let structure = query :: List.concat_map field_helper_items fields in
  A.pstr_module ~loc
    (A.module_binding ~loc ~name:{ loc; txt = Some module_name }
       ~expr:(A.pmod_structure ~loc structure))

let generate_str ~loc:_ ~path:_ (_rec_flag, tds) =
  List.concat_map (fun td -> [ gen_entity td; gen_query_module td ]) tds

let gen_sig_for_type td =
  let loc = loc_of_type_decl td in
  let typ = A.ptyp_constr ~loc (lid ~loc [ "Ent_ocaml"; "entity" ]) [] in
  [
    A.psig_value ~loc
      (A.value_description ~loc
         ~name:{ loc; txt = td.ptype_name.txt ^ "_entity" }
         ~type_:typ ~prim:[]);
  ]

let generate_sig ~loc:_ ~path:_ (_rec_flag, tds) =
  List.concat_map gen_sig_for_type tds

let deriver =
  Deriving.add "ent"
    ~str_type_decl:(Deriving.Generator.make_noarg generate_str)
    ~sig_type_decl:(Deriving.Generator.make_noarg generate_sig)

let expand_entity ~ctxt:_ name =
  let loc = name.pexp_loc in
  match name.pexp_desc with
  | Pexp_constant (Pconst_string (entity_name, _, _)) -> str ~loc entity_name
  | _ -> Location.raise_errorf ~loc "%%ent.entity expects a string literal"

let entity_extension =
  Extension.V3.declare "ent.entity" Extension.Context.expression
    Ast_pattern.(single_expr_payload __)
    expand_entity

let () =
  ignore deriver;
  Driver.register_transformation "ent-ocaml-ppx"
    ~rules:[ Context_free.Rule.extension entity_extension ]
