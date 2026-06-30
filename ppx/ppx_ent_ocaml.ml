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
let unit ~loc = A.pexp_construct ~loc (lid ~loc [ "()" ]) None
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

let ent_index_attr =
  Attribute.declare "ent.index" Attribute.Context.label_declaration
    Ast_pattern.(single_expr_payload (estring __))
    (fun name -> name)

let ent_indexes_attr =
  Attribute.declare "ent.indexes" Attribute.Context.type_declaration
    Ast_pattern.(single_expr_payload (elist __))
    (fun indexes -> indexes)

let ent_edges_attr =
  Attribute.declare "ent.edges" Attribute.Context.type_declaration
    Ast_pattern.(single_expr_payload (elist __))
    (fun edges -> edges)

let ent_query_rules_attr =
  Attribute.declare "ent.query_rules" Attribute.Context.type_declaration
    Ast_pattern.(single_expr_payload (elist __))
    (fun rules -> rules)

let ent_mutation_rules_attr =
  Attribute.declare "ent.mutation_rules" Attribute.Context.type_declaration
    Ast_pattern.(single_expr_payload (elist __))
    (fun rules -> rules)

let ent_mutation_hooks_attr =
  Attribute.declare "ent.mutation_hooks" Attribute.Context.type_declaration
    Ast_pattern.(single_expr_payload (elist __))
    (fun hooks -> hooks)

let ent_query_interceptors_attr =
  Attribute.declare "ent.query_interceptors" Attribute.Context.type_declaration
    Ast_pattern.(single_expr_payload (elist __))
    (fun interceptors -> interceptors)

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

let ent_json_attr =
  Attribute.declare "ent.json" Attribute.Context.label_declaration
    Ast_pattern.(pstr nil)
    ()

let ent_default_attr =
  Attribute.declare "ent.default" Attribute.Context.label_declaration
    Ast_pattern.(single_expr_payload __)
    (fun expr -> expr)

let ent_default_result_attr =
  Attribute.declare "ent.default_result" Attribute.Context.label_declaration
    Ast_pattern.(single_expr_payload __)
    (fun expr -> expr)

let ent_update_default_attr =
  Attribute.declare "ent.update_default" Attribute.Context.label_declaration
    Ast_pattern.(single_expr_payload __)
    (fun expr -> expr)

let ent_update_default_result_attr =
  Attribute.declare "ent.update_default_result" Attribute.Context.label_declaration
    Ast_pattern.(single_expr_payload __)
    (fun expr -> expr)

let ent_validate_attr =
  Attribute.declare "ent.validate" Attribute.Context.label_declaration
    Ast_pattern.(single_expr_payload (elist __))
    (fun validators -> validators)

let ent_sensitive_attr =
  Attribute.declare "ent.sensitive" Attribute.Context.label_declaration
    Ast_pattern.(pstr nil)
    ()

let ent_deprecated_attr =
  Attribute.declare "ent.deprecated" Attribute.Context.label_declaration
    Ast_pattern.(single_expr_payload (estring __))
    (fun reason -> reason)

let ent_comment_attr =
  Attribute.declare "ent.comment" Attribute.Context.label_declaration
    Ast_pattern.(single_expr_payload (estring __))
    (fun comment -> comment)

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

let label_name = function
  | Longident.Lident name -> name
  | Ldot (_, name) -> name
  | Lapply _ -> Location.raise_errorf "ent index record labels cannot be applicative paths"

let parse_bool_expr expr =
  match expr.pexp_desc with
  | Pexp_construct ({ txt = Lident "true"; _ }, None) -> true
  | Pexp_construct ({ txt = Lident "false"; _ }, None) -> false
  | _ -> Location.raise_errorf ~loc:expr.pexp_loc "ent index unique must be true or false"

let parse_string_expr ~what expr =
  match expr.pexp_desc with
  | Pexp_constant (Pconst_string (value, _, _)) -> value
  | _ -> Location.raise_errorf ~loc:expr.pexp_loc "ent %s must be a string" what

let parse_name_expr expr =
  match expr.pexp_desc with
  | Pexp_constant (Pconst_string (name, _, _)) -> Some name
  | Pexp_construct ({ txt = Lident "None"; _ }, None) -> None
  | Pexp_construct
      ({ txt = Lident "Some"; _ }, Some { pexp_desc = Pexp_constant (Pconst_string (name, _, _)); _ }) ->
      Some name
  | _ ->
      Location.raise_errorf ~loc:expr.pexp_loc
        "ent index name must be a string, None, or Some string"

let parse_string_list_expr ~what expr =
  match expr.pexp_desc with
  | Pexp_construct ({ txt = Lident "[]"; _ }, None) -> []
  | Pexp_array _ ->
      Location.raise_errorf ~loc:expr.pexp_loc
        "ent %s must use list syntax, not array syntax" what
  | _ ->
      let rec loop acc expr =
        match expr.pexp_desc with
        | Pexp_construct ({ txt = Lident "[]"; _ }, None) -> List.rev acc
        | Pexp_construct
            ( { txt = Lident "::"; _ },
              Some
                {
                  pexp_desc =
                    Pexp_tuple
                      [
                        { pexp_desc = Pexp_constant (Pconst_string (value, _, _)); _ };
                        rest;
                      ];
                  _;
                } ) ->
            loop (value :: acc) rest
        | _ ->
            Location.raise_errorf ~loc:expr.pexp_loc
              "ent %s must be a string list" what
      in
      loop [] expr

let parse_index_spec expr =
  match expr.pexp_desc with
  | Pexp_record (fields, None) ->
      let name = ref None in
      let index_fields = ref None in
      let edges = ref (Some []) in
      let unique = ref None in
      let partial_filter = ref (list ~loc:expr.pexp_loc []) in
      List.iter
        (fun (label, value) ->
          match label_name label.txt with
          | "name" -> name := Some (parse_name_expr value)
          | "fields" ->
              index_fields :=
                Some (parse_string_list_expr ~what:"index fields" value)
          | "edges" ->
              edges := Some (parse_string_list_expr ~what:"index edges" value)
          | "unique" -> unique := Some (parse_bool_expr value)
          | "partial_filter" -> partial_filter := value
          | field ->
              Location.raise_errorf ~loc:value.pexp_loc
                "unknown ent index option: %s" field)
        fields;
      let index_fields =
        match !index_fields with
        | Some fields -> fields
        | None ->
            Location.raise_errorf ~loc:expr.pexp_loc
              "ent index record requires fields"
      in
      let unique =
        match !unique with
        | Some unique -> unique
        | None ->
            Location.raise_errorf ~loc:expr.pexp_loc
              "ent index record requires unique"
      in
      A.pexp_record ~loc:expr.pexp_loc
        [
          ( lid ~loc:expr.pexp_loc [ "Ent_ocaml"; "name" ],
            option ~loc:expr.pexp_loc
              (match !name with
              | None -> None
              | Some name -> Option.map (str ~loc:expr.pexp_loc) name) );
          ( lid ~loc:expr.pexp_loc [ "Ent_ocaml"; "fields" ],
            list ~loc:expr.pexp_loc
              (List.map (str ~loc:expr.pexp_loc) index_fields) );
          ( lid ~loc:expr.pexp_loc [ "Ent_ocaml"; "edges" ],
            list ~loc:expr.pexp_loc
              (List.map (str ~loc:expr.pexp_loc) (Option.value !edges ~default:[])) );
          (lid ~loc:expr.pexp_loc [ "Ent_ocaml"; "unique" ], bool ~loc:expr.pexp_loc unique);
          (lid ~loc:expr.pexp_loc [ "Ent_ocaml"; "partial_filter" ], !partial_filter);
        ]
        None
  | _ ->
      Location.raise_errorf ~loc:expr.pexp_loc
        "ent index must be a record: { name = \"...\"; fields = [ ... ]; unique = false }"

let parse_edge_cardinality ~loc = function
  | "one" -> constr ~loc [ "Ent_ocaml"; "One" ]
  | "many" -> constr ~loc [ "Ent_ocaml"; "Many" ]
  | value ->
      Location.raise_errorf ~loc
        "ent edge cardinality must be \"one\" or \"many\", got %S" value

let parse_edge_direction ~loc ?ref_name = function
  | "to" -> constr ~loc [ "Ent_ocaml"; "To" ]
  | "from" ->
      let ref_name =
        match ref_name with
        | Some ref_name -> ref_name
        | None ->
            Location.raise_errorf ~loc
              "ent from-edge requires ref_name = \"...\""
      in
      constr_arg ~loc [ "Ent_ocaml"; "From" ]
        (A.pexp_record ~loc
           [
             (lid ~loc [ "ref_name" ], str ~loc ref_name);
           ]
           None)
  | value ->
      Location.raise_errorf ~loc
        "ent edge direction must be \"to\" or \"from\", got %S" value

let parse_edge_spec expr =
  match expr.pexp_desc with
  | Pexp_record (fields, None) ->
      let name = ref None in
      let target = ref None in
      let direction = ref "to" in
      let cardinality = ref None in
      let required = ref (Some false) in
      let storage_key = ref None in
      let ref_name = ref None in
      List.iter
        (fun (label, value) ->
          match label_name label.txt with
          | "name" -> name := Some (parse_string_expr ~what:"edge name" value)
          | "target" ->
              target := Some (parse_string_expr ~what:"edge target" value)
          | "direction" ->
              direction := parse_string_expr ~what:"edge direction" value
          | "cardinality" ->
              cardinality :=
                Some (parse_string_expr ~what:"edge cardinality" value)
          | "required" -> required := Some (parse_bool_expr value)
          | "storage_key" ->
              storage_key :=
                Some (parse_string_expr ~what:"edge storage_key" value)
          | "ref_name" ->
              ref_name := Some (parse_string_expr ~what:"edge ref_name" value)
          | field ->
              Location.raise_errorf ~loc:value.pexp_loc
                "unknown ent edge option: %s" field)
        fields;
      let loc = expr.pexp_loc in
      let required =
        match !required with
        | Some required -> required
        | None -> false
      in
      let name =
        match !name with
        | Some name -> name
        | None -> Location.raise_errorf ~loc "ent edge record requires name"
      in
      let target =
        match !target with
        | Some target -> target
        | None -> Location.raise_errorf ~loc "ent edge record requires target"
      in
      let cardinality =
        match !cardinality with
        | Some cardinality -> cardinality
        | None -> Location.raise_errorf ~loc "ent edge record requires cardinality"
      in
      A.pexp_record ~loc
        [
          (lid ~loc [ "Ent_ocaml"; "name" ], str ~loc name);
          (lid ~loc [ "Ent_ocaml"; "target" ], str ~loc target);
          ( lid ~loc [ "Ent_ocaml"; "direction" ],
            parse_edge_direction ~loc ?ref_name:!ref_name !direction );
          ( lid ~loc [ "Ent_ocaml"; "cardinality" ],
            parse_edge_cardinality ~loc cardinality );
          (lid ~loc [ "Ent_ocaml"; "required" ], bool ~loc required);
          ( lid ~loc [ "Ent_ocaml"; "storage_key" ],
            option ~loc (Option.map (str ~loc) !storage_key) );
        ]
        None
  | _ ->
      Location.raise_errorf ~loc:expr.pexp_loc
        "ent edge must be a record: { name = \"...\"; target = \"...\"; storage_key = \"...\"; cardinality = \"one\" }"

let parse_edge_name expr =
  match expr.pexp_desc with
  | Pexp_record (fields, None) -> (
      match
        List.find_map
          (fun (label, value) ->
            match label_name label.txt with
            | "name" -> Some (parse_string_expr ~what:"edge name" value)
            | _ -> None)
          fields
      with
      | Some name -> name
      | None -> Location.raise_errorf ~loc:expr.pexp_loc "ent edge record requires name")
  | _ ->
      Location.raise_errorf ~loc:expr.pexp_loc
        "ent edge must be a record"

let edge_specs td =
  Attribute.get ent_edges_attr td |> Option.value ~default:[]

let edge_names td = edge_specs td |> List.map parse_edge_name

let type_path_name path =
  match List.rev (type_path_parts path) with
  | [] -> invalid_arg "type_path_name"
  | name :: _ -> name

let rec field_type_expr ~loc field =
  match Attribute.get ent_enum_attr field with
  | Some values ->
      constr_arg ~loc [ "Ent_ocaml"; "Enum" ]
        (list ~loc (List.map (str ~loc) values))
  | None when has_attr ent_json_attr field -> constr ~loc [ "Ent_ocaml"; "Json" ]
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

let is_json_field field =
  has_attr ent_json_attr field
  ||
  match field.pld_type.ptyp_desc with
  | Ptyp_constr ({ txt = path; _ }, []) ->
      let name = String.concat "." (type_path_parts path) in
      name = "Yojson.Safe.t" || name = "Yojson.t"
  | _ -> false

let is_comparable_field field =
  if is_option field then false
  else
    match Attribute.get ent_enum_attr field with
    | Some _ -> false
    | None -> (
        match field.pld_type.ptyp_desc with
        | Ptyp_constr ({ txt = Longident.Lident "string"; _ }, [])
        | Ptyp_constr ({ txt = Longident.Lident "int"; _ }, [])
        | Ptyp_constr ({ txt = Longident.Lident "int32"; _ }, [])
        | Ptyp_constr
            ( { txt = Longident.Ldot (Longident.Lident "Int32", "t"); _ },
              [] )
        | Ptyp_constr ({ txt = Longident.Lident "int64"; _ }, [])
        | Ptyp_constr
            ( { txt = Longident.Ldot (Longident.Lident "Int64", "t"); _ },
              [] )
        | Ptyp_constr ({ txt = Longident.Lident "float"; _ }, []) ->
            true
        | Ptyp_constr ({ txt = path; _ }, []) ->
            let name = String.concat "." (type_path_parts path) in
            name = "Ptime.t" || name = "Uuidm.t"
        | _ -> false)

let rec value_constructor field =
  match Attribute.get ent_enum_attr field with
  | Some _ -> Some [ "Ent_ocaml"; "V_string" ]
  | None when has_attr ent_json_attr field -> None
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
      | Ptyp_constr ({ txt = Longident.Lident "list"; _ }, [ inner ])
      | Ptyp_constr
          ({ txt = Longident.Ldot (Longident.Lident "List", "t"); _ }, [ inner ])
        ->
          let inner = { field with pld_type = inner } in
          Option.map (fun _ -> [ "Ent_ocaml"; "V_list" ]) (value_constructor inner)
      | Ptyp_constr ({ txt = path; _ }, []) ->
          let name = String.concat "." (type_path_parts path) in
          if name = "Ptime.t" || name = "Uuidm.t" || name = "Yojson.Safe.t"
             || name = "Yojson.t"
          then None
          else Some [ "Ent_ocaml"; "V_doc" ]
      | _ -> None)

let rec value_expr ~loc field value =
  match Attribute.get ent_enum_attr field with
  | Some _ -> constr_arg ~loc [ "Ent_ocaml"; "V_string" ] value
  | None when has_attr ent_json_attr field -> value
  | None -> (
      match field.pld_type.ptyp_desc with
      | Ptyp_constr ({ txt = Longident.Lident "string"; _ }, []) ->
          constr_arg ~loc [ "Ent_ocaml"; "V_string" ] value
      | Ptyp_constr ({ txt = Longident.Lident "int"; _ }, []) ->
          constr_arg ~loc [ "Ent_ocaml"; "V_int" ] value
      | Ptyp_constr ({ txt = Longident.Lident "int32"; _ }, [])
      | Ptyp_constr
          ({ txt = Longident.Ldot (Longident.Lident "Int32", "t"); _ }, []) ->
          constr_arg ~loc [ "Ent_ocaml"; "V_int32" ] value
      | Ptyp_constr ({ txt = Longident.Lident "int64"; _ }, [])
      | Ptyp_constr
          ({ txt = Longident.Ldot (Longident.Lident "Int64", "t"); _ }, []) ->
          constr_arg ~loc [ "Ent_ocaml"; "V_int64" ] value
      | Ptyp_constr ({ txt = Longident.Lident "float"; _ }, []) ->
          constr_arg ~loc [ "Ent_ocaml"; "V_float" ] value
      | Ptyp_constr ({ txt = Longident.Lident "bool"; _ }, []) ->
          constr_arg ~loc [ "Ent_ocaml"; "V_bool" ] value
      | Ptyp_constr ({ txt = Longident.Lident "option"; _ }, [ inner ])
      | Ptyp_constr
          ({ txt = Longident.Ldot (Longident.Lident "Option", "t"); _ }, [ inner ])
        ->
          A.pexp_match ~loc value
            [
              A.case
                ~lhs:(A.ppat_construct ~loc (lid ~loc [ "None" ]) None)
                ~guard:None ~rhs:(constr ~loc [ "Ent_ocaml"; "V_null" ]);
              A.case
                ~lhs:
                  (A.ppat_construct ~loc (lid ~loc [ "Some" ])
                     (Some (pvar ~loc "value")))
                ~guard:None
                ~rhs:(value_expr ~loc { field with pld_type = inner } (evar ~loc "value"));
            ]
      | Ptyp_constr ({ txt = Longident.Lident "list"; _ }, [ inner ])
      | Ptyp_constr
          ({ txt = Longident.Ldot (Longident.Lident "List", "t"); _ }, [ inner ])
        ->
          let inner = { field with pld_type = inner } in
          let mapper =
            A.pexp_fun ~loc Nolabel None (pvar ~loc "value")
              (value_expr ~loc inner (evar ~loc "value"))
          in
          constr_arg ~loc [ "Ent_ocaml"; "V_list" ]
            (app ~loc (ident ~loc [ "List"; "map" ]) [ mapper; value ])
      | Ptyp_constr ({ txt = path; _ }, []) ->
          let name = String.concat "." (type_path_parts path) in
          if name = "Ptime.t" || name = "Uuidm.t" || name = "Yojson.Safe.t"
             || name = "Yojson.t"
          then
            Location.raise_errorf ~loc:field.pld_type.ptyp_loc
              "ent deriving cannot generate value helper for this field type"
          else app ~loc (evar ~loc (type_path_name path ^ "_to_ent_value")) [ value ]
      | _ ->
          Location.raise_errorf ~loc:field.pld_type.ptyp_loc
            "ent deriving cannot generate value helper for this field type")

let ok ~loc expr = constr_arg ~loc [ "Ok" ] expr
let error_string ~loc message = constr_arg ~loc [ "Error" ] (str ~loc message)

let rec value_decoder_expr ~loc field value =
  match Attribute.get ent_enum_attr field with
  | Some _ ->
      A.pexp_match ~loc value
        [
          A.case
            ~lhs:
              (A.ppat_construct ~loc (lid ~loc [ "Ent_ocaml"; "V_string" ])
                 (Some (pvar ~loc "value")))
            ~guard:None ~rhs:(ok ~loc (evar ~loc "value"));
          A.case ~lhs:(A.ppat_any ~loc) ~guard:None
            ~rhs:
              (error_string ~loc
                 ("decoder type mismatch for field: " ^ field.pld_name.txt));
        ]
  | None when has_attr ent_json_attr field -> ok ~loc value
  | None -> (
      match field.pld_type.ptyp_desc with
      | Ptyp_constr ({ txt = Longident.Lident "string"; _ }, []) ->
          A.pexp_match ~loc value
            [
              A.case
                ~lhs:
                  (A.ppat_construct ~loc (lid ~loc [ "Ent_ocaml"; "V_string" ])
                     (Some (pvar ~loc "value")))
                ~guard:None ~rhs:(ok ~loc (evar ~loc "value"));
              A.case ~lhs:(A.ppat_any ~loc) ~guard:None
                ~rhs:
                  (error_string ~loc
                     ("decoder type mismatch for field: " ^ field.pld_name.txt));
            ]
      | Ptyp_constr ({ txt = Longident.Lident "int"; _ }, []) ->
          A.pexp_match ~loc value
            [
              A.case
                ~lhs:
                  (A.ppat_construct ~loc (lid ~loc [ "Ent_ocaml"; "V_int" ])
                     (Some (pvar ~loc "value")))
                ~guard:None ~rhs:(ok ~loc (evar ~loc "value"));
              A.case ~lhs:(A.ppat_any ~loc) ~guard:None
                ~rhs:
                  (error_string ~loc
                     ("decoder type mismatch for field: " ^ field.pld_name.txt));
            ]
      | Ptyp_constr ({ txt = Longident.Lident "int32"; _ }, [])
      | Ptyp_constr
          ({ txt = Longident.Ldot (Longident.Lident "Int32", "t"); _ }, []) ->
          A.pexp_match ~loc value
            [
              A.case
                ~lhs:
                  (A.ppat_construct ~loc (lid ~loc [ "Ent_ocaml"; "V_int32" ])
                     (Some (pvar ~loc "value")))
                ~guard:None ~rhs:(ok ~loc (evar ~loc "value"));
              A.case ~lhs:(A.ppat_any ~loc) ~guard:None
                ~rhs:
                  (error_string ~loc
                     ("decoder type mismatch for field: " ^ field.pld_name.txt));
            ]
      | Ptyp_constr ({ txt = Longident.Lident "int64"; _ }, [])
      | Ptyp_constr
          ({ txt = Longident.Ldot (Longident.Lident "Int64", "t"); _ }, []) ->
          A.pexp_match ~loc value
            [
              A.case
                ~lhs:
                  (A.ppat_construct ~loc (lid ~loc [ "Ent_ocaml"; "V_int64" ])
                     (Some (pvar ~loc "value")))
                ~guard:None ~rhs:(ok ~loc (evar ~loc "value"));
              A.case ~lhs:(A.ppat_any ~loc) ~guard:None
                ~rhs:
                  (error_string ~loc
                     ("decoder type mismatch for field: " ^ field.pld_name.txt));
            ]
      | Ptyp_constr ({ txt = Longident.Lident "float"; _ }, []) ->
          A.pexp_match ~loc value
            [
              A.case
                ~lhs:
                  (A.ppat_construct ~loc (lid ~loc [ "Ent_ocaml"; "V_float" ])
                     (Some (pvar ~loc "value")))
                ~guard:None ~rhs:(ok ~loc (evar ~loc "value"));
              A.case ~lhs:(A.ppat_any ~loc) ~guard:None
                ~rhs:
                  (error_string ~loc
                     ("decoder type mismatch for field: " ^ field.pld_name.txt));
            ]
      | Ptyp_constr ({ txt = Longident.Lident "bool"; _ }, []) ->
          A.pexp_match ~loc value
            [
              A.case
                ~lhs:
                  (A.ppat_construct ~loc (lid ~loc [ "Ent_ocaml"; "V_bool" ])
                     (Some (pvar ~loc "value")))
                ~guard:None ~rhs:(ok ~loc (evar ~loc "value"));
              A.case ~lhs:(A.ppat_any ~loc) ~guard:None
                ~rhs:
                  (error_string ~loc
                     ("decoder type mismatch for field: " ^ field.pld_name.txt));
            ]
      | Ptyp_constr ({ txt = Longident.Lident "option"; _ }, [ inner ])
      | Ptyp_constr
          ({ txt = Longident.Ldot (Longident.Lident "Option", "t"); _ }, [ inner ])
        ->
          A.pexp_match ~loc value
            [
              A.case
                ~lhs:
                  (A.ppat_construct ~loc (lid ~loc [ "Ent_ocaml"; "V_null" ])
                     None)
                ~guard:None ~rhs:(ok ~loc (constr ~loc [ "None" ]));
              A.case
                ~lhs:(pvar ~loc "value")
                ~guard:None
                ~rhs:
                  (A.pexp_match ~loc
                     (value_decoder_expr ~loc { field with pld_type = inner }
                        (evar ~loc "value"))
                     [
                       A.case
                         ~lhs:
                           (A.ppat_construct ~loc (lid ~loc [ "Ok" ])
                              (Some (pvar ~loc "value")))
                         ~guard:None
                         ~rhs:
                           (ok ~loc
                              (A.pexp_construct ~loc (lid ~loc [ "Some" ])
                                 (Some (evar ~loc "value"))));
                       A.case
                         ~lhs:
                           (A.ppat_construct ~loc (lid ~loc [ "Error" ])
                              (Some (pvar ~loc "error")))
                         ~guard:None
                         ~rhs:(constr_arg ~loc [ "Error" ] (evar ~loc "error"));
                     ]);
            ]
      | Ptyp_constr ({ txt = Longident.Lident "list"; _ }, [ inner ])
      | Ptyp_constr
          ({ txt = Longident.Ldot (Longident.Lident "List", "t"); _ }, [ inner ])
        ->
          let inner = { field with pld_type = inner } in
          A.pexp_match ~loc value
            [
              A.case
                ~lhs:
                  (A.ppat_construct ~loc (lid ~loc [ "Ent_ocaml"; "V_list" ])
                     (Some (pvar ~loc "values")))
                ~guard:None
                ~rhs:
                  (A.pexp_let ~loc Recursive
                     [
                       A.value_binding ~loc ~pat:(pvar ~loc "loop")
                         ~expr:
                           (A.pexp_fun ~loc Nolabel None (pvar ~loc "acc")
                              (A.pexp_fun ~loc Nolabel None (pvar ~loc "values")
                                 (A.pexp_match ~loc (evar ~loc "values")
                                    [
                                      A.case
                                        ~lhs:
                                          (A.ppat_construct ~loc (lid ~loc [ "[]" ])
                                             None)
                                        ~guard:None
                                        ~rhs:
                                          (ok ~loc
                                             (app ~loc
                                                (ident ~loc [ "List"; "rev" ])
                                                [ evar ~loc "acc" ]));
                                      A.case
                                        ~lhs:
                                          (A.ppat_construct ~loc (lid ~loc [ "::" ])
                                             (Some
                                                (A.ppat_tuple ~loc
                                                   [
                                                     pvar ~loc "value";
                                                     pvar ~loc "rest";
                                                   ])))
                                        ~guard:None
                                        ~rhs:
                                          (A.pexp_match ~loc
                                             (value_decoder_expr ~loc inner
                                                (evar ~loc "value"))
                                             [
                                               A.case
                                                 ~lhs:
                                                   (A.ppat_construct ~loc
                                                      (lid ~loc [ "Ok" ])
                                                      (Some (pvar ~loc "value")))
                                                 ~guard:None
                                                 ~rhs:
                                                   (app ~loc (evar ~loc "loop")
                                                      [
                                                        A.pexp_construct ~loc
                                                          (lid ~loc [ "::" ])
                                                          (Some
                                                             (A.pexp_tuple ~loc
                                                                [
                                                                  evar ~loc "value";
                                                                  evar ~loc "acc";
                                                                ]));
                                                        evar ~loc "rest";
                                                      ]);
                                               A.case
                                                 ~lhs:
                                                   (A.ppat_construct ~loc
                                                      (lid ~loc [ "Error" ])
                                                      (Some (pvar ~loc "error")))
                                                 ~guard:None
                                                 ~rhs:
                                                   (constr_arg ~loc [ "Error" ]
                                                      (evar ~loc "error"));
                                             ]);
                                    ])));
                     ]
                     (app ~loc (evar ~loc "loop")
                        [ list ~loc []; evar ~loc "values" ]));
              A.case ~lhs:(A.ppat_any ~loc) ~guard:None
                ~rhs:
                  (error_string ~loc
                     ("decoder type mismatch for field: " ^ field.pld_name.txt));
            ]
      | Ptyp_constr ({ txt = path; _ }, []) ->
          let name = String.concat "." (type_path_parts path) in
          if name = "Ptime.t" || name = "Uuidm.t" || name = "Yojson.Safe.t"
             || name = "Yojson.t"
          then ok ~loc value
          else app ~loc (evar ~loc (type_path_name path ^ "_of_ent_value")) [ value ]
      | _ ->
          Location.raise_errorf ~loc:field.pld_type.ptyp_loc
            "ent deriving cannot generate typed decoder for this field type")

let validator_expr ~loc field validator =
  A.pexp_fun ~loc Nolabel None (pvar ~loc "ent_value")
    (A.pexp_match ~loc
       (value_decoder_expr ~loc field (evar ~loc "ent_value"))
       [
         A.case
           ~lhs:
             (A.ppat_construct ~loc (lid ~loc [ "Ok" ])
                (Some (pvar ~loc "value")))
           ~guard:None
           ~rhs:(app ~loc validator [ evar ~loc "value" ]);
         A.case
           ~lhs:
             (A.ppat_construct ~loc (lid ~loc [ "Error" ])
                (Some (pvar ~loc "error")))
           ~guard:None ~rhs:(constr_arg ~loc [ "Error" ] (evar ~loc "error"));
       ])

let validators_expr ~loc field =
  match Attribute.get ent_validate_attr field with
  | None -> list ~loc []
  | Some validators ->
      list ~loc (List.map (validator_expr ~loc field) validators)

let predicate_function ~loc name constructor field_name field =
  let value = evar ~loc "value" in
  let body =
    constr_arg ~loc constructor
      (A.pexp_tuple ~loc
         [
           str ~loc field_name;
           value_expr ~loc field value;
         ])
  in
  A.pstr_value ~loc Nonrecursive
    [
      A.value_binding ~loc
        ~pat:(pvar ~loc name)
        ~expr:(A.pexp_fun ~loc Nolabel None (pvar ~loc "value") body);
    ]

let list_predicate_function ~loc name constructor field_name field =
  let values = evar ~loc "values" in
  let mapper =
    A.pexp_fun ~loc Nolabel None (pvar ~loc "value")
      (value_expr ~loc field (evar ~loc "value"))
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
        (lid ~loc [ "Ent_ocaml"; "value_alias" ], evar ~loc "as_");
      ]
      None
  in
  A.pstr_value ~loc Nonrecursive
    [
      A.value_binding ~loc
        ~pat:(pvar ~loc (field_name ^ "_order"))
        ~expr:
          (A.pexp_fun ~loc (Optional "direction") None (pvar ~loc "direction")
             (A.pexp_fun ~loc (Optional "as_") None (pvar ~loc "as_")
                (A.pexp_fun ~loc Nolabel None (unit_pat ~loc) body)));
    ]

let selector_function ~loc field_name =
  A.pstr_value ~loc Nonrecursive
    [
      A.value_binding ~loc ~pat:(pvar ~loc ("select_" ^ field_name))
        ~expr:(str ~loc field_name);
    ]

let cursor_function ~loc name core_name field_name field =
  let direction =
    A.pexp_apply ~loc
      (ident ~loc [ "Option"; "value" ])
      [
        (Nolabel, evar ~loc "direction");
        (Labelled "default", constr ~loc [ "Ent_ocaml"; "Asc" ]);
      ]
  in
  let body =
    A.pexp_let ~loc Nonrecursive
      [
        A.value_binding ~loc ~pat:(pvar ~loc "direction")
          ~expr:direction;
      ]
      (A.pexp_apply ~loc
         (ident ~loc [ "Ent_ocaml"; "Query"; core_name ])
         [
           (Labelled "field", str ~loc field_name);
           (Labelled "direction", evar ~loc "direction");
           (Nolabel, value_expr ~loc field (evar ~loc "value"));
           (Nolabel, evar ~loc "query");
         ])
  in
  A.pstr_value ~loc Nonrecursive
    [
      A.value_binding ~loc ~pat:(pvar ~loc name)
        ~expr:
          (A.pexp_fun ~loc (Optional "direction") None (pvar ~loc "direction")
             (A.pexp_fun ~loc Nolabel None (pvar ~loc "value")
                (A.pexp_fun ~loc Nolabel None (pvar ~loc "query") body)));
    ]

let cursor_term_function ~loc field_name field =
  let direction =
    A.pexp_apply ~loc
      (ident ~loc [ "Option"; "value" ])
      [
        (Nolabel, evar ~loc "direction");
        (Labelled "default", constr ~loc [ "Ent_ocaml"; "Asc" ]);
      ]
  in
  let body =
    A.pexp_let ~loc Nonrecursive
      [ A.value_binding ~loc ~pat:(pvar ~loc "direction") ~expr:direction ]
      (A.pexp_record ~loc
         [
           (lid ~loc [ "Ent_ocaml"; "field" ], str ~loc field_name);
           (lid ~loc [ "Ent_ocaml"; "direction" ], evar ~loc "direction");
           ( lid ~loc [ "Ent_ocaml"; "value" ],
             value_expr ~loc field (evar ~loc "value") );
         ]
         None)
  in
  A.pstr_value ~loc Nonrecursive
    [
      A.value_binding ~loc ~pat:(pvar ~loc (field_name ^ "_cursor"))
        ~expr:
          (A.pexp_fun ~loc (Optional "direction") None (pvar ~loc "direction")
             (A.pexp_fun ~loc Nolabel None (pvar ~loc "value") body));
    ]

let json_value_predicate_function ~loc name constructor field_name =
  A.pstr_value ~loc Nonrecursive
    [
      A.value_binding ~loc ~pat:(pvar ~loc name)
        ~expr:
          (A.pexp_fun ~loc Nolabel None (pvar ~loc "path")
             (A.pexp_fun ~loc Nolabel None (pvar ~loc "value")
                (constr_arg ~loc constructor
                   (A.pexp_tuple ~loc
                      [ str ~loc field_name; evar ~loc "path"; evar ~loc "value" ]))));
    ]

let json_list_predicate_function ~loc name constructor field_name =
  A.pstr_value ~loc Nonrecursive
    [
      A.value_binding ~loc ~pat:(pvar ~loc name)
        ~expr:
          (A.pexp_fun ~loc Nolabel None (pvar ~loc "path")
             (A.pexp_fun ~loc Nolabel None (pvar ~loc "values")
                (constr_arg ~loc constructor
                   (A.pexp_tuple ~loc
                      [ str ~loc field_name; evar ~loc "path"; evar ~loc "values" ]))));
    ]

let json_nullary_predicate_function ~loc name constructor field_name =
  A.pstr_value ~loc Nonrecursive
    [
      A.value_binding ~loc ~pat:(pvar ~loc name)
        ~expr:
          (A.pexp_fun ~loc Nolabel None (pvar ~loc "path")
             (constr_arg ~loc constructor
                (A.pexp_tuple ~loc [ str ~loc field_name; evar ~loc "path" ])));
    ]

let json_order_function ~loc field_name =
  let direction =
    A.pexp_apply ~loc
      (ident ~loc [ "Option"; "value" ])
      [
        (Nolabel, evar ~loc "direction");
        (Labelled "default", constr ~loc [ "Ent_ocaml"; "Asc" ]);
      ]
  in
  let field =
    app ~loc (ident ~loc [ "String"; "concat" ])
      [
        str ~loc ".";
        A.pexp_construct ~loc (lid ~loc [ "::" ])
          (Some (A.pexp_tuple ~loc [ str ~loc field_name; evar ~loc "path" ]));
      ]
  in
  let body =
    A.pexp_let ~loc Nonrecursive
      [ A.value_binding ~loc ~pat:(pvar ~loc "direction") ~expr:direction ]
      (A.pexp_record ~loc
         [
           (lid ~loc [ "Ent_ocaml"; "field" ], field);
           (lid ~loc [ "Ent_ocaml"; "direction" ], evar ~loc "direction");
           (lid ~loc [ "Ent_ocaml"; "value_alias" ], evar ~loc "as_");
         ]
         None)
  in
  A.pstr_value ~loc Nonrecursive
    [
      A.value_binding ~loc ~pat:(pvar ~loc (field_name ^ "_path_order"))
        ~expr:
          (A.pexp_fun ~loc (Optional "direction") None (pvar ~loc "direction")
             (A.pexp_fun ~loc (Optional "as_") None (pvar ~loc "as_")
                (A.pexp_fun ~loc Nolabel None (pvar ~loc "path") body)));
    ]

let field_helper_items field =
  let loc = field.pld_loc in
  let field_name = field.pld_name.txt in
  let order = order_function ~loc field_name in
  let selector = selector_function ~loc field_name in
  let value_item =
    match value_constructor field with
    | None -> []
    | Some _ ->
        [
          A.pstr_value ~loc Nonrecursive
            [
              A.value_binding ~loc ~pat:(pvar ~loc field_name)
                ~expr:
                  (A.pexp_fun ~loc Nolabel None (pvar ~loc "value")
                     (A.pexp_tuple ~loc
                        [
                          str ~loc field_name;
                          value_expr ~loc field (evar ~loc "value");
                        ]));
            ];
        ]
  in
  let json_helpers =
    if is_json_field field then
      [
        json_value_predicate_function ~loc (field_name ^ "_path_eq")
          [ "Ent_ocaml"; "Json_eq" ] field_name;
        json_value_predicate_function ~loc (field_name ^ "_path_neq")
          [ "Ent_ocaml"; "Json_neq" ] field_name;
        json_value_predicate_function ~loc (field_name ^ "_path_gt")
          [ "Ent_ocaml"; "Json_gt" ] field_name;
        json_value_predicate_function ~loc (field_name ^ "_path_gte")
          [ "Ent_ocaml"; "Json_gte" ] field_name;
        json_value_predicate_function ~loc (field_name ^ "_path_lt")
          [ "Ent_ocaml"; "Json_lt" ] field_name;
        json_value_predicate_function ~loc (field_name ^ "_path_lte")
          [ "Ent_ocaml"; "Json_lte" ] field_name;
        json_list_predicate_function ~loc (field_name ^ "_path_in")
          [ "Ent_ocaml"; "Json_in" ] field_name;
        json_list_predicate_function ~loc (field_name ^ "_path_not_in")
          [ "Ent_ocaml"; "Json_not_in" ] field_name;
        json_nullary_predicate_function ~loc (field_name ^ "_path_is_nil")
          [ "Ent_ocaml"; "Json_is_nil" ] field_name;
        json_nullary_predicate_function ~loc (field_name ^ "_path_not_nil")
          [ "Ent_ocaml"; "Json_not_nil" ] field_name;
        json_order_function ~loc field_name;
      ]
    else []
  in
  match value_constructor field with
  | None -> value_item @ [ selector; order ] @ json_helpers
  | Some value_path ->
      let base =
        [
          predicate_function ~loc (field_name ^ "_eq") [ "Ent_ocaml"; "Eq" ]
            field_name field;
          predicate_function ~loc (field_name ^ "_neq") [ "Ent_ocaml"; "Neq" ]
            field_name field;
          list_predicate_function ~loc (field_name ^ "_in")
            [ "Ent_ocaml"; "In" ] field_name field;
          list_predicate_function ~loc (field_name ^ "_not_in")
            [ "Ent_ocaml"; "Not_in" ] field_name field;
          selector;
          order;
        ]
      in
      let comparison_helpers =
        if is_comparable_field field then
          [
            predicate_function ~loc (field_name ^ "_gt")
              [ "Ent_ocaml"; "Gt" ] field_name field;
            predicate_function ~loc (field_name ^ "_gte")
              [ "Ent_ocaml"; "Gte" ] field_name field;
            predicate_function ~loc (field_name ^ "_lt")
              [ "Ent_ocaml"; "Lt" ] field_name field;
            predicate_function ~loc (field_name ^ "_lte")
              [ "Ent_ocaml"; "Lte" ] field_name field;
            cursor_function ~loc ("after_" ^ field_name) "after" field_name field;
            cursor_function ~loc ("before_" ^ field_name) "before" field_name field;
            cursor_term_function ~loc field_name field;
          ]
        else []
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
      value_item @ base @ comparison_helpers @ nil_helpers @ string_helpers
      @ json_helpers

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
      (lid ~loc [ "Ent_ocaml"; "validators" ], validators_expr ~loc field);
      (lid ~loc [ "Ent_ocaml"; "sensitive" ], bool ~loc (has_attr ent_sensitive_attr field));
      ( lid ~loc [ "Ent_ocaml"; "deprecated" ],
        option ~loc (Option.map (str ~loc) (Attribute.get ent_deprecated_attr field)) );
      ( lid ~loc [ "Ent_ocaml"; "comment" ],
        option ~loc (Option.map (str ~loc) (Attribute.get ent_comment_attr field)) );
    ]
    None

let index_expr ~loc ~collection field =
  let field_name = field.pld_name.txt in
  let storage_key =
    Option.value (Attribute.get ent_key_attr field) ~default:field_name
  in
  let unique = has_attr ent_unique_attr field in
  let indexed = Attribute.get ent_index_attr field in
  let name =
    match indexed with
    | Some name -> Some name
    | None when unique -> Some ("unique_" ^ collection ^ "_" ^ field_name)
    | None -> None
  in
  match (unique, indexed) with
  | true, None when storage_key = "_id" -> None
  | false, None -> None
  | unique, _ ->
      Some
        (A.pexp_record ~loc
           [
             (lid ~loc [ "Ent_ocaml"; "name" ], option ~loc (Option.map (str ~loc) name));
             (lid ~loc [ "Ent_ocaml"; "fields" ], list ~loc [ str ~loc field_name ]);
             (lid ~loc [ "Ent_ocaml"; "edges" ], list ~loc []);
             (lid ~loc [ "Ent_ocaml"; "unique" ], bool ~loc unique);
             (lid ~loc [ "Ent_ocaml"; "partial_filter" ], list ~loc []);
           ]
           None)

let ensure_record td =
  if td.ptype_params <> [] then
    Location.raise_errorf ~loc:td.ptype_loc "ent deriving does not support parameterized entity records";
  match td.ptype_kind with
  | Ptype_record fields -> fields
  | _ -> Location.raise_errorf ~loc:td.ptype_loc "ent deriving supports record entity types only"

let default_binding ~loc field body =
  match Attribute.get ent_default_attr field with
  | None -> body
  | Some default ->
      if Option.is_some (Attribute.get ent_default_result_attr field) then
        Location.raise_errorf ~loc:field.pld_loc
          "ent field cannot use both default and default_result";
      if has_attr ent_unique_attr field then
        Location.raise_errorf ~loc:field.pld_loc
          "ent default fields cannot be unique";
      let field_name = field.pld_name.txt in
      let default_pair =
        A.pexp_tuple ~loc
          [ str ~loc field_name; value_expr ~loc field default ]
      in
      let default_fields =
        A.pexp_construct ~loc (lid ~loc [ "::" ])
          (Some (A.pexp_tuple ~loc [ default_pair; evar ~loc "fields" ]))
      in
      let fields =
        A.pexp_ifthenelse ~loc
          (app ~loc (ident ~loc [ "List"; "mem_assoc" ])
             [ str ~loc field_name; evar ~loc "fields" ])
          (evar ~loc "fields") (Some default_fields)
      in
      A.pexp_let ~loc Nonrecursive
        [
          A.value_binding ~loc ~pat:(pvar ~loc "fields")
            ~expr:fields;
        ]
        body

let apply_default_bindings ~loc fields body =
  List.fold_right (default_binding ~loc) fields body

let default_result_binding ~loc field body =
  match Attribute.get ent_default_result_attr field with
  | None -> body
  | Some default ->
      if has_attr ent_unique_attr field then
        Location.raise_errorf ~loc:field.pld_loc
          "ent default_result fields cannot be unique";
      let field_name = field.pld_name.txt in
      let default_pair =
        A.pexp_tuple ~loc
          [ str ~loc field_name; value_expr ~loc field (evar ~loc "value") ]
      in
      let default_fields =
        A.pexp_construct ~loc (lid ~loc [ "::" ])
          (Some (A.pexp_tuple ~loc [ default_pair; evar ~loc "fields" ]))
      in
      let apply_default =
        A.pexp_match ~loc default
          [
            A.case
              ~lhs:
                (A.ppat_construct ~loc (lid ~loc [ "Ok" ])
                   (Some (pvar ~loc "value")))
              ~guard:None
              ~rhs:
                (A.pexp_let ~loc Nonrecursive
                   [
                     A.value_binding ~loc ~pat:(pvar ~loc "fields")
                       ~expr:default_fields;
                   ]
                   body);
            A.case
              ~lhs:
                (A.ppat_construct ~loc (lid ~loc [ "Error" ])
                   (Some (pvar ~loc "error")))
              ~guard:None
              ~rhs:(constr_arg ~loc [ "Error" ] (evar ~loc "error"));
          ]
      in
      A.pexp_ifthenelse ~loc
        (app ~loc (ident ~loc [ "List"; "mem_assoc" ])
           [ str ~loc field_name; evar ~loc "fields" ])
        body (Some apply_default)

let apply_default_result_bindings ~loc fields body =
  List.fold_right (default_result_binding ~loc) fields body

let update_default_binding ~loc field body =
  match Attribute.get ent_update_default_attr field with
  | None -> body
  | Some default ->
      if Option.is_some (Attribute.get ent_update_default_result_attr field) then
        Location.raise_errorf ~loc:field.pld_loc
          "ent field cannot use both update_default and update_default_result";
      if has_attr ent_unique_attr field then
        Location.raise_errorf ~loc:field.pld_loc
          "ent update_default fields cannot be unique";
      if has_attr ent_immutable_attr field then
        Location.raise_errorf ~loc:field.pld_loc
          "ent update_default fields cannot be immutable";
      let field_name = field.pld_name.txt in
      let default_pair =
        A.pexp_tuple ~loc
          [ str ~loc field_name; value_expr ~loc field default ]
      in
      let default_set =
        A.pexp_construct ~loc (lid ~loc [ "::" ])
          (Some (A.pexp_tuple ~loc [ default_pair; evar ~loc "set" ]))
      in
      let touched =
        app ~loc (ident ~loc [ "||" ])
          [
            app ~loc (ident ~loc [ "List"; "mem_assoc" ])
              [ str ~loc field_name; evar ~loc "set" ];
            app ~loc (ident ~loc [ "||" ])
              [
                app ~loc (ident ~loc [ "List"; "mem_assoc" ])
                  [ str ~loc field_name; evar ~loc "add" ];
                app ~loc (ident ~loc [ "List"; "mem" ])
                  [ str ~loc field_name; evar ~loc "clear" ];
              ];
          ]
      in
      let set =
        A.pexp_ifthenelse ~loc touched (evar ~loc "set") (Some default_set)
      in
      A.pexp_let ~loc Nonrecursive
        [ A.value_binding ~loc ~pat:(pvar ~loc "set") ~expr:set ]
        body

let apply_update_default_bindings ~loc fields body =
  List.fold_right (update_default_binding ~loc) fields body

let update_default_result_binding ~loc field body =
  match Attribute.get ent_update_default_result_attr field with
  | None -> body
  | Some default ->
      if has_attr ent_unique_attr field then
        Location.raise_errorf ~loc:field.pld_loc
          "ent update_default_result fields cannot be unique";
      if has_attr ent_immutable_attr field then
        Location.raise_errorf ~loc:field.pld_loc
          "ent update_default_result fields cannot be immutable";
      let field_name = field.pld_name.txt in
      let default_pair =
        A.pexp_tuple ~loc
          [ str ~loc field_name; value_expr ~loc field (evar ~loc "value") ]
      in
      let default_set =
        A.pexp_construct ~loc (lid ~loc [ "::" ])
          (Some (A.pexp_tuple ~loc [ default_pair; evar ~loc "set" ]))
      in
      let touched =
        app ~loc (ident ~loc [ "||" ])
          [
            app ~loc (ident ~loc [ "List"; "mem_assoc" ])
              [ str ~loc field_name; evar ~loc "set" ];
            app ~loc (ident ~loc [ "||" ])
              [
                app ~loc (ident ~loc [ "List"; "mem_assoc" ])
                  [ str ~loc field_name; evar ~loc "add" ];
                app ~loc (ident ~loc [ "List"; "mem" ])
                  [ str ~loc field_name; evar ~loc "clear" ];
              ];
          ]
      in
      let apply_default =
        A.pexp_match ~loc default
          [
            A.case
              ~lhs:
                (A.ppat_construct ~loc (lid ~loc [ "Ok" ])
                   (Some (pvar ~loc "value")))
              ~guard:None
              ~rhs:
                (A.pexp_let ~loc Nonrecursive
                   [ A.value_binding ~loc ~pat:(pvar ~loc "set") ~expr:default_set ]
                   body);
            A.case
              ~lhs:
                (A.ppat_construct ~loc (lid ~loc [ "Error" ])
                   (Some (pvar ~loc "error")))
              ~guard:None
              ~rhs:(constr_arg ~loc [ "Error" ] (evar ~loc "error"));
          ]
      in
      A.pexp_ifthenelse ~loc touched body (Some apply_default)

let apply_update_default_result_bindings ~loc fields body =
  List.fold_right (update_default_result_binding ~loc) fields body

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
  let indexes =
    (fields |> List.filter_map (index_expr ~loc ~collection))
    @ (Attribute.get ent_indexes_attr td
      |> Option.value ~default:[]
      |> List.map parse_index_spec)
  in
  let edges =
    Attribute.get ent_edges_attr td
    |> Option.value ~default:[]
    |> List.map parse_edge_spec
  in
  let expr =
    A.pexp_record ~loc
      [
        (lid ~loc [ "Ent_ocaml"; "name" ], str ~loc entity_name);
        (lid ~loc [ "Ent_ocaml"; "collection" ], str ~loc collection);
        (lid ~loc [ "Ent_ocaml"; "fields" ], list ~loc (List.map field_expr fields));
        (lid ~loc [ "Ent_ocaml"; "edges" ], list ~loc edges);
        (lid ~loc [ "Ent_ocaml"; "indexes" ], list ~loc indexes);
      ]
      None
  in
  A.pstr_value ~loc Nonrecursive
    [ A.value_binding ~loc ~pat:(pvar ~loc (type_name ^ "_entity")) ~expr ]

let gen_schema_snapshot td =
  let loc = loc_of_type_decl td in
  let type_name = td.ptype_name.txt in
  let expr =
    app ~loc
      (ident ~loc [ "Ent_ocaml"; "Schema_snapshot"; "entity" ])
      [ evar ~loc (type_name ^ "_entity") ]
  in
  A.pstr_value ~loc Nonrecursive
    [ A.value_binding ~loc ~pat:(pvar ~loc (type_name ^ "_schema_snapshot")) ~expr ]

let gen_value_converter td =
  let loc = loc_of_type_decl td in
  let fields = ensure_record td in
  let type_name = td.ptype_name.txt in
  let value = evar ~loc "value" in
  let value_pat =
    A.ppat_constraint ~loc (pvar ~loc "value")
      (A.ptyp_constr ~loc (lid ~loc [ type_name ]) [])
  in
  let field_value field =
    let field_name = field.pld_name.txt in
    let access =
      A.pexp_field ~loc value (lid ~loc [ field_name ])
    in
    A.pexp_tuple ~loc [ str ~loc field_name; value_expr ~loc field access ]
  in
  let record_expr =
    A.pexp_record ~loc
      (List.map
         (fun field ->
           let field_name = field.pld_name.txt in
           (lid ~loc [ field_name ], evar ~loc field_name))
         fields)
      None
  in
  let rec decode_fields fields body =
    match fields with
    | [] -> ok ~loc body
    | field :: rest ->
        let field_name = field.pld_name.txt in
        let missing =
          if is_option field then ok ~loc (constr ~loc [ "None" ])
          else error_string ~loc ("missing field: " ^ field_name)
        in
        let field_result =
          A.pexp_match ~loc
            (app ~loc (ident ~loc [ "List"; "assoc_opt" ])
               [ str ~loc field_name; evar ~loc "fields" ])
            [
              A.case
                ~lhs:(A.ppat_construct ~loc (lid ~loc [ "None" ]) None)
                ~guard:None ~rhs:missing;
              A.case
                ~lhs:
                  (A.ppat_construct ~loc (lid ~loc [ "Some" ])
                     (Some (pvar ~loc "value")))
                ~guard:None
                ~rhs:(value_decoder_expr ~loc field (evar ~loc "value"));
            ]
        in
        A.pexp_match ~loc field_result
          [
            A.case
              ~lhs:
                (A.ppat_construct ~loc (lid ~loc [ "Ok" ])
                   (Some (pvar ~loc field_name)))
              ~guard:None ~rhs:(decode_fields rest body);
            A.case
              ~lhs:
                (A.ppat_construct ~loc (lid ~loc [ "Error" ])
                   (Some (pvar ~loc "error")))
              ~guard:None ~rhs:(constr_arg ~loc [ "Error" ] (evar ~loc "error"));
          ]
  in
  A.pstr_value ~loc Nonrecursive
    [
      A.value_binding ~loc ~pat:(pvar ~loc (type_name ^ "_to_ent_value"))
        ~expr:
          (A.pexp_fun ~loc Nolabel None value_pat
             (constr_arg ~loc [ "Ent_ocaml"; "V_doc" ]
                (list ~loc (List.map field_value fields))));
      A.value_binding ~loc ~pat:(pvar ~loc (type_name ^ "_of_ent_value"))
        ~expr:
          (A.pexp_fun ~loc Nolabel None (pvar ~loc "value")
             (A.pexp_match ~loc (evar ~loc "value")
                [
                  A.case
                    ~lhs:
                      (A.ppat_construct ~loc (lid ~loc [ "Ent_ocaml"; "V_doc" ])
                         (Some (pvar ~loc "fields")))
                    ~guard:None ~rhs:(decode_fields fields record_expr);
                  A.case ~lhs:(A.ppat_any ~loc) ~guard:None
                    ~rhs:
                      (error_string ~loc
                         ("decoder type mismatch for record: " ^ type_name));
                ]));
    ]

let gen_query_module td =
  let loc = loc_of_type_decl td in
  let fields = ensure_record td in
  let edges = edge_names td in
  let type_name = td.ptype_name.txt in
  let module_name = snake_to_pascal type_name in
  let schema_query_rules =
    Attribute.get ent_query_rules_attr td |> Option.value ~default:[]
  in
  let schema_mutation_rules =
    Attribute.get ent_mutation_rules_attr td |> Option.value ~default:[]
  in
  let schema_mutation_hooks =
    Attribute.get ent_mutation_hooks_attr td |> Option.value ~default:[]
  in
  let schema_query_interceptors =
    Attribute.get ent_query_interceptors_attr td |> Option.value ~default:[]
  in
  let query_body =
    A.pexp_record ~loc
      [
        (lid ~loc [ "Ent_ocaml"; "entity" ], evar ~loc (type_name ^ "_entity"));
        (lid ~loc [ "Ent_ocaml"; "predicates" ], evar ~loc "where");
        (lid ~loc [ "Ent_ocaml"; "select" ], evar ~loc "select");
        (lid ~loc [ "Ent_ocaml"; "orders" ], evar ~loc "order");
        (lid ~loc [ "Ent_ocaml"; "limit" ], evar ~loc "limit");
        (lid ~loc [ "Ent_ocaml"; "offset" ], evar ~loc "offset");
      ]
      None
  in
  let query_pat =
    A.ppat_constraint ~loc (pvar ~loc "query")
      (A.ptyp_constr ~loc (lid ~loc [ "Ent_ocaml"; "query" ]) [])
  in
  let query =
    A.pstr_value ~loc Nonrecursive
      [
        A.value_binding ~loc ~pat:(pvar ~loc "query")
          ~expr:
            (A.pexp_fun ~loc (Optional "where") (Some (list ~loc []))
               (pvar ~loc "where")
               (A.pexp_fun ~loc (Optional "select") (Some (list ~loc []))
                  (pvar ~loc "select")
                  (A.pexp_fun ~loc (Optional "order") (Some (list ~loc []))
                     (pvar ~loc "order")
                     (A.pexp_fun ~loc (Optional "limit") None
                        (pvar ~loc "limit")
                        (A.pexp_fun ~loc (Optional "offset") None
                           (pvar ~loc "offset")
                           (A.pexp_fun ~loc Nolabel None (unit_pat ~loc)
                              query_body))))));
      ]
  in
  let boolean_predicates =
    A.pstr_value ~loc Nonrecursive
      [
        A.value_binding ~loc ~pat:(pvar ~loc "and_")
          ~expr:
            (A.pexp_fun ~loc Nolabel None (pvar ~loc "predicates")
               (constr_arg ~loc [ "Ent_ocaml"; "And" ]
                  (evar ~loc "predicates")));
        A.value_binding ~loc ~pat:(pvar ~loc "or_")
          ~expr:
            (A.pexp_fun ~loc Nolabel None (pvar ~loc "predicates")
               (constr_arg ~loc [ "Ent_ocaml"; "Or" ]
                  (evar ~loc "predicates")));
        A.value_binding ~loc ~pat:(pvar ~loc "not_")
          ~expr:
            (A.pexp_fun ~loc Nolabel None (pvar ~loc "predicate")
               (constr_arg ~loc [ "Ent_ocaml"; "Not" ]
                  (evar ~loc "predicate")));
        A.value_binding ~loc ~pat:(pvar ~loc "dynamic_filter")
          ~expr:
            (A.pexp_fun ~loc (Optional "value") None (pvar ~loc "value")
               (A.pexp_fun ~loc (Labelled "field") None (pvar ~loc "field")
                  (A.pexp_fun ~loc Nolabel None (pvar ~loc "op")
                     (A.pexp_apply ~loc
                        (ident ~loc [ "Ent_ocaml"; "Dynamic_filter"; "make" ])
                        [
                          (Optional "value", evar ~loc "value");
                          (Labelled "field", evar ~loc "field");
                          (Nolabel, evar ~loc "op");
                        ]))));
        A.value_binding ~loc ~pat:(pvar ~loc "dynamic_predicate")
          ~expr:
            (A.pexp_fun ~loc Nolabel None (pvar ~loc "filter")
               (app ~loc
                  (ident ~loc [ "Ent_ocaml"; "Dynamic_filter"; "predicate" ])
                  [ evar ~loc (type_name ^ "_entity"); evar ~loc "filter" ]));
        A.value_binding ~loc ~pat:(pvar ~loc "entql_predicate")
          ~expr:
            (A.pexp_fun ~loc Nolabel None (pvar ~loc "expression")
               (app ~loc (ident ~loc [ "Ent_ocaml"; "Entql"; "predicate" ])
                  [ evar ~loc (type_name ^ "_entity"); evar ~loc "expression" ]));
      ]
  in
  let query_pipe_helpers =
    A.pstr_value ~loc Nonrecursive
      [
        A.value_binding ~loc ~pat:(pvar ~loc "where")
          ~expr:
            (A.pexp_fun ~loc Nolabel None (pvar ~loc "predicate")
               (A.pexp_fun ~loc Nolabel None query_pat
                  (A.pexp_record ~loc
                     [
                       ( lid ~loc [ "Ent_ocaml"; "predicates" ],
                         app ~loc (ident ~loc [ "@" ])
                           [
                             A.pexp_field ~loc (evar ~loc "query")
                               (lid ~loc [ "Ent_ocaml"; "predicates" ]);
                             list ~loc [ evar ~loc "predicate" ];
                           ] );
                     ]
                     (Some (evar ~loc "query")))));
        A.value_binding ~loc ~pat:(pvar ~loc "where_all")
          ~expr:
            (A.pexp_fun ~loc Nolabel None (pvar ~loc "predicates")
               (A.pexp_fun ~loc Nolabel None query_pat
                  (A.pexp_record ~loc
                     [
                       ( lid ~loc [ "Ent_ocaml"; "predicates" ],
                         app ~loc (ident ~loc [ "@" ])
                           [
                             A.pexp_field ~loc (evar ~loc "query")
                               (lid ~loc [ "Ent_ocaml"; "predicates" ]);
                             evar ~loc "predicates";
                           ] );
                     ]
                     (Some (evar ~loc "query")))));
        A.value_binding ~loc ~pat:(pvar ~loc "where_dynamic")
          ~expr:
            (A.pexp_fun ~loc Nolabel None (pvar ~loc "filter")
               (A.pexp_fun ~loc Nolabel None query_pat
                  (app ~loc
                     (ident ~loc [ "Ent_ocaml"; "Dynamic_filter"; "where" ])
                     [ evar ~loc "filter"; evar ~loc "query" ])));
        A.value_binding ~loc ~pat:(pvar ~loc "where_dynamic_all")
          ~expr:
            (A.pexp_fun ~loc Nolabel None (pvar ~loc "filters")
               (A.pexp_fun ~loc Nolabel None query_pat
                  (app ~loc
                     (ident ~loc [ "Ent_ocaml"; "Dynamic_filter"; "where_all" ])
                     [ evar ~loc "filters"; evar ~loc "query" ])));
        A.value_binding ~loc ~pat:(pvar ~loc "where_entql")
          ~expr:
            (A.pexp_fun ~loc Nolabel None (pvar ~loc "expression")
               (A.pexp_fun ~loc Nolabel None query_pat
                  (app ~loc (ident ~loc [ "Ent_ocaml"; "Entql"; "where" ])
                     [ evar ~loc "expression"; evar ~loc "query" ])));
        A.value_binding ~loc ~pat:(pvar ~loc "select")
          ~expr:
            (A.pexp_fun ~loc Nolabel None (pvar ~loc "fields")
               (A.pexp_fun ~loc Nolabel None query_pat
                  (A.pexp_record ~loc
                     [ (lid ~loc [ "Ent_ocaml"; "select" ], evar ~loc "fields") ]
                     (Some (evar ~loc "query")))));
        A.value_binding ~loc ~pat:(pvar ~loc "order_by")
          ~expr:
            (A.pexp_fun ~loc Nolabel None (pvar ~loc "orders")
               (A.pexp_fun ~loc Nolabel None query_pat
                  (A.pexp_record ~loc
                     [ (lid ~loc [ "Ent_ocaml"; "orders" ], evar ~loc "orders") ]
                     (Some (evar ~loc "query")))));
        A.value_binding ~loc ~pat:(pvar ~loc "limit")
          ~expr:
            (A.pexp_fun ~loc Nolabel None (pvar ~loc "limit")
               (A.pexp_fun ~loc Nolabel None query_pat
                  (A.pexp_record ~loc
                     [
                       ( lid ~loc [ "Ent_ocaml"; "limit" ],
                         constr_arg ~loc [ "Some" ] (evar ~loc "limit") );
                     ]
                     (Some (evar ~loc "query")))));
        A.value_binding ~loc ~pat:(pvar ~loc "offset")
          ~expr:
            (A.pexp_fun ~loc Nolabel None (pvar ~loc "offset")
               (A.pexp_fun ~loc Nolabel None query_pat
                  (A.pexp_record ~loc
                     [
                       ( lid ~loc [ "Ent_ocaml"; "offset" ],
                         constr_arg ~loc [ "Some" ] (evar ~loc "offset") );
                     ]
                     (Some (evar ~loc "query")))));
        A.value_binding ~loc ~pat:(pvar ~loc "after_cursor")
          ~expr:
            (A.pexp_fun ~loc Nolabel None (pvar ~loc "terms")
               (A.pexp_fun ~loc Nolabel None query_pat
                  (app ~loc
                     (ident ~loc [ "Ent_ocaml"; "Query"; "after_cursor" ])
                     [ evar ~loc "terms"; evar ~loc "query" ])));
        A.value_binding ~loc ~pat:(pvar ~loc "before_cursor")
          ~expr:
            (A.pexp_fun ~loc Nolabel None (pvar ~loc "terms")
               (A.pexp_fun ~loc Nolabel None query_pat
                  (app ~loc
                     (ident ~loc [ "Ent_ocaml"; "Query"; "before_cursor" ])
                     [ evar ~loc "terms"; evar ~loc "query" ])));
      ]
  in
  let aggregate_helpers =
    A.pstr_value ~loc Nonrecursive
      [
        A.value_binding ~loc ~pat:(pvar ~loc "count")
          ~expr:
            (A.pexp_fun ~loc Nolabel None (pvar ~loc "query")
               (app ~loc (ident ~loc [ "Ent_ocaml"; "Aggregate"; "count" ])
                  [ evar ~loc "query" ]));
        A.value_binding ~loc ~pat:(pvar ~loc "min")
          ~expr:
            (A.pexp_fun ~loc Nolabel None (pvar ~loc "field")
               (A.pexp_fun ~loc Nolabel None (pvar ~loc "query")
                  (app ~loc (ident ~loc [ "Ent_ocaml"; "Aggregate"; "min" ])
                     [ evar ~loc "field"; evar ~loc "query" ])));
        A.value_binding ~loc ~pat:(pvar ~loc "max")
          ~expr:
            (A.pexp_fun ~loc Nolabel None (pvar ~loc "field")
               (A.pexp_fun ~loc Nolabel None (pvar ~loc "query")
                  (app ~loc (ident ~loc [ "Ent_ocaml"; "Aggregate"; "max" ])
                     [ evar ~loc "field"; evar ~loc "query" ])));
        A.value_binding ~loc ~pat:(pvar ~loc "sum")
          ~expr:
            (A.pexp_fun ~loc Nolabel None (pvar ~loc "field")
               (A.pexp_fun ~loc Nolabel None (pvar ~loc "query")
                  (app ~loc (ident ~loc [ "Ent_ocaml"; "Aggregate"; "sum" ])
                     [ evar ~loc "field"; evar ~loc "query" ])));
        A.value_binding ~loc ~pat:(pvar ~loc "avg")
          ~expr:
            (A.pexp_fun ~loc Nolabel None (pvar ~loc "field")
               (A.pexp_fun ~loc Nolabel None (pvar ~loc "query")
                  (app ~loc (ident ~loc [ "Ent_ocaml"; "Aggregate"; "avg" ])
                     [ evar ~loc "field"; evar ~loc "query" ])));
        A.value_binding ~loc ~pat:(pvar ~loc "count_as")
          ~expr:
            (A.pexp_fun ~loc Nolabel None (pvar ~loc "name")
               (app ~loc (ident ~loc [ "Ent_ocaml"; "Aggregate"; "count_as" ])
                  [ evar ~loc "name" ]));
        A.value_binding ~loc ~pat:(pvar ~loc "min_as")
          ~expr:
            (A.pexp_fun ~loc Nolabel None (pvar ~loc "name")
               (A.pexp_fun ~loc Nolabel None (pvar ~loc "field")
                  (app ~loc (ident ~loc [ "Ent_ocaml"; "Aggregate"; "min_as" ])
                     [ evar ~loc "name"; evar ~loc "field" ])));
        A.value_binding ~loc ~pat:(pvar ~loc "max_as")
          ~expr:
            (A.pexp_fun ~loc Nolabel None (pvar ~loc "name")
               (A.pexp_fun ~loc Nolabel None (pvar ~loc "field")
                  (app ~loc (ident ~loc [ "Ent_ocaml"; "Aggregate"; "max_as" ])
                     [ evar ~loc "name"; evar ~loc "field" ])));
        A.value_binding ~loc ~pat:(pvar ~loc "sum_as")
          ~expr:
            (A.pexp_fun ~loc Nolabel None (pvar ~loc "name")
               (A.pexp_fun ~loc Nolabel None (pvar ~loc "field")
                  (app ~loc (ident ~loc [ "Ent_ocaml"; "Aggregate"; "sum_as" ])
                     [ evar ~loc "name"; evar ~loc "field" ])));
        A.value_binding ~loc ~pat:(pvar ~loc "avg_as")
          ~expr:
            (A.pexp_fun ~loc Nolabel None (pvar ~loc "name")
               (A.pexp_fun ~loc Nolabel None (pvar ~loc "field")
                  (app ~loc (ident ~loc [ "Ent_ocaml"; "Aggregate"; "avg_as" ])
                     [ evar ~loc "name"; evar ~loc "field" ])));
        A.value_binding ~loc ~pat:(pvar ~loc "scan")
          ~expr:
            (A.pexp_fun ~loc Nolabel None (pvar ~loc "ops")
               (A.pexp_fun ~loc Nolabel None (pvar ~loc "query")
                  (app ~loc (ident ~loc [ "Ent_ocaml"; "Aggregate"; "scan" ])
                     [ evar ~loc "ops"; evar ~loc "query" ])));
        A.value_binding ~loc ~pat:(pvar ~loc "group_by")
          ~expr:
            (A.pexp_fun ~loc Nolabel None (pvar ~loc "field")
               (A.pexp_fun ~loc Nolabel None (pvar ~loc "aggregate")
                  (app ~loc
                     (ident ~loc [ "Ent_ocaml"; "Aggregate"; "group_by" ])
                     [ evar ~loc "field"; evar ~loc "aggregate" ])));
      ]
  in
  let edge_helper_items edge_name =
    [
      A.pstr_value ~loc Nonrecursive
        [
          A.value_binding ~loc ~pat:(pvar ~loc ("has_" ^ edge_name))
            ~expr:
              (A.pexp_fun ~loc Nolabel None (unit_pat ~loc)
                 (constr_arg ~loc [ "Ent_ocaml"; "Has_edge" ]
                    (str ~loc edge_name)));
        ];
      A.pstr_value ~loc Nonrecursive
        [
          A.value_binding ~loc ~pat:(pvar ~loc ("has_" ^ edge_name ^ "_with"))
            ~expr:
              (A.pexp_fun ~loc Nolabel None (pvar ~loc "predicates")
                 (constr_arg ~loc [ "Ent_ocaml"; "Has_edge_with" ]
                    (A.pexp_tuple ~loc
                       [ str ~loc edge_name; evar ~loc "predicates" ])));
        ];
      A.pstr_value ~loc Nonrecursive
        [
          A.value_binding ~loc ~pat:(pvar ~loc edge_name)
            ~expr:
              (A.pexp_fun ~loc Nolabel None (pvar ~loc "predicate")
                 (constr_arg ~loc [ "Ent_ocaml"; "Has_edge_with" ]
                    (A.pexp_tuple ~loc
                       [ str ~loc edge_name; list ~loc [ evar ~loc "predicate" ] ])));
        ];
      A.pstr_value ~loc Nonrecursive
        [
          A.value_binding ~loc ~pat:(pvar ~loc ("query_" ^ edge_name))
            ~expr:
              (A.pexp_fun ~loc (Optional "target_query") None
                 (pvar ~loc "target_query")
                 (A.pexp_fun ~loc (Labelled "target") None
                    (pvar ~loc "target")
                    (A.pexp_fun ~loc Nolabel None query_pat
                       (A.pexp_apply ~loc
                          (ident ~loc [ "Ent_ocaml"; "Edge_query"; "make" ])
                          [
                            (Optional "target_query", evar ~loc "target_query");
                            (Labelled "edge", str ~loc edge_name);
                            (Labelled "target", evar ~loc "target");
                            (Nolabel, evar ~loc "query");
                          ]))));
        ];
      A.pstr_value ~loc Nonrecursive
        [
          A.value_binding ~loc ~pat:(pvar ~loc ("with_" ^ edge_name))
            ~expr:(evar ~loc ("query_" ^ edge_name));
        ];
    ]
  in
  let mutation_record ~op ~predicates ~set ~clear ~add ~on_insert =
    A.pexp_record ~loc
      [
        (lid ~loc [ "Ent_ocaml"; "entity" ], evar ~loc (type_name ^ "_entity"));
        (lid ~loc [ "Ent_ocaml"; "op" ], constr ~loc [ "Ent_ocaml"; op ]);
        (lid ~loc [ "Ent_ocaml"; "predicates" ], predicates);
        (lid ~loc [ "Ent_ocaml"; "set" ], set);
        (lid ~loc [ "Ent_ocaml"; "clear" ], clear);
        (lid ~loc [ "Ent_ocaml"; "add" ], add);
        (lid ~loc [ "Ent_ocaml"; "on_insert" ], on_insert);
      ]
      None
  in
  let create =
    let body =
      mutation_record ~op:"Create" ~predicates:(list ~loc [])
        ~set:(evar ~loc "fields") ~clear:(list ~loc [])
        ~add:(list ~loc []) ~on_insert:(list ~loc [])
    in
    let body =
      A.pexp_let ~loc Nonrecursive
        [
          A.value_binding ~loc ~pat:(pvar ~loc "fields")
            ~expr:(list ~loc []);
        ]
        (apply_default_bindings ~loc fields body)
    in
    A.pstr_value ~loc Nonrecursive
      [
        A.value_binding ~loc ~pat:(pvar ~loc "create")
          ~expr:
            (A.pexp_fun ~loc Nolabel None (unit_pat ~loc) body);
      ]
  in
  let create_values =
    let body =
      mutation_record ~op:"Create" ~predicates:(list ~loc [])
        ~set:(evar ~loc "fields") ~clear:(list ~loc [])
        ~add:(list ~loc []) ~on_insert:(list ~loc [])
    in
    A.pstr_value ~loc Nonrecursive
      [
        A.value_binding ~loc ~pat:(pvar ~loc "create_values")
          ~expr:
            (A.pexp_fun ~loc Nolabel None (pvar ~loc "fields")
               (apply_default_bindings ~loc fields body));
      ]
  in
  let create_result =
    let body =
      mutation_record ~op:"Create" ~predicates:(list ~loc [])
        ~set:(evar ~loc "fields") ~clear:(list ~loc [])
        ~add:(list ~loc []) ~on_insert:(list ~loc [])
      |> constr_arg ~loc [ "Ok" ]
    in
    let body =
      A.pexp_let ~loc Nonrecursive
        [ A.value_binding ~loc ~pat:(pvar ~loc "fields") ~expr:(list ~loc []) ]
        (apply_default_bindings ~loc fields
           (apply_default_result_bindings ~loc fields body))
    in
    A.pstr_value ~loc Nonrecursive
      [
        A.value_binding ~loc ~pat:(pvar ~loc "create_result")
          ~expr:(A.pexp_fun ~loc Nolabel None (unit_pat ~loc) body);
      ]
  in
  let create_values_result =
    let body =
      mutation_record ~op:"Create" ~predicates:(list ~loc [])
        ~set:(evar ~loc "fields") ~clear:(list ~loc [])
        ~add:(list ~loc []) ~on_insert:(list ~loc [])
      |> constr_arg ~loc [ "Ok" ]
    in
    A.pstr_value ~loc Nonrecursive
      [
        A.value_binding ~loc ~pat:(pvar ~loc "create_values_result")
          ~expr:
            (A.pexp_fun ~loc Nolabel None (pvar ~loc "fields")
               (apply_default_bindings ~loc fields
                  (apply_default_result_bindings ~loc fields body)));
      ]
  in
  let create_record =
    let fields_expr =
      A.pexp_match ~loc
        (app ~loc (evar ~loc (type_name ^ "_to_ent_value"))
           [ evar ~loc "value" ])
        [
          A.case ~lhs:(A.ppat_construct ~loc (lid ~loc [ "Ent_ocaml"; "V_doc" ]) (Some (pvar ~loc "fields")))
            ~guard:None ~rhs:(evar ~loc "fields");
          A.case ~lhs:(A.ppat_any ~loc) ~guard:None
            ~rhs:(list ~loc []);
        ]
    in
    let body =
      app ~loc (evar ~loc "create_values") [ fields_expr ]
    in
    A.pstr_value ~loc Nonrecursive
      [
        A.value_binding ~loc ~pat:(pvar ~loc "create_record")
          ~expr:(A.pexp_fun ~loc Nolabel None (pvar ~loc "value") body);
      ]
  in
  let create_record_result =
    let fields_expr =
      A.pexp_match ~loc
        (app ~loc (evar ~loc (type_name ^ "_to_ent_value"))
           [ evar ~loc "value" ])
        [
          A.case ~lhs:(A.ppat_construct ~loc (lid ~loc [ "Ent_ocaml"; "V_doc" ]) (Some (pvar ~loc "fields")))
            ~guard:None ~rhs:(evar ~loc "fields");
          A.case ~lhs:(A.ppat_any ~loc) ~guard:None
            ~rhs:(list ~loc []);
        ]
    in
    let body = app ~loc (evar ~loc "create_values_result") [ fields_expr ] in
    A.pstr_value ~loc Nonrecursive
      [
        A.value_binding ~loc ~pat:(pvar ~loc "create_record_result")
          ~expr:(A.pexp_fun ~loc Nolabel None (pvar ~loc "value") body);
      ]
  in
  let create_many =
    A.pstr_value ~loc Nonrecursive
      [
        A.value_binding ~loc ~pat:(pvar ~loc "create_many")
          ~expr:
            (A.pexp_fun ~loc Nolabel None (pvar ~loc "rows")
               (app ~loc (ident ~loc [ "List"; "map" ])
                  [ evar ~loc "create_record"; evar ~loc "rows" ]));
      ]
  in
  let result_list_fn name item_fn =
    A.pstr_value ~loc Recursive
      [
        A.value_binding ~loc ~pat:(pvar ~loc name)
          ~expr:
            (A.pexp_fun ~loc Nolabel None (pvar ~loc "rows")
               (A.pexp_let ~loc Recursive
                  [
                    A.value_binding ~loc ~pat:(pvar ~loc "loop")
                      ~expr:
                        (A.pexp_fun ~loc Nolabel None (pvar ~loc "acc")
                           (A.pexp_fun ~loc Nolabel None (pvar ~loc "rows")
                              (A.pexp_match ~loc (evar ~loc "rows")
                                 [
                                   A.case
                                     ~lhs:(A.ppat_construct ~loc (lid ~loc [ "[]" ]) None)
                                     ~guard:None
                                     ~rhs:
                                       (constr_arg ~loc [ "Ok" ]
                                          (app ~loc (ident ~loc [ "List"; "rev" ])
                                             [ evar ~loc "acc" ]));
                                   A.case
                                     ~lhs:
                                       (A.ppat_construct ~loc (lid ~loc [ "::" ])
                                          (Some
                                             (A.ppat_tuple ~loc
                                                [ pvar ~loc "row"; pvar ~loc "rest" ])))
                                     ~guard:None
                                     ~rhs:
                                       (A.pexp_match ~loc
                                          (app ~loc (evar ~loc item_fn)
                                             [ evar ~loc "row" ])
                                          [
                                            A.case
                                              ~lhs:
                                                (A.ppat_construct ~loc
                                                   (lid ~loc [ "Ok" ])
                                                   (Some (pvar ~loc "mutation")))
                                              ~guard:None
                                              ~rhs:
                                                (app ~loc (evar ~loc "loop")
                                                   [
                                                     A.pexp_construct ~loc
                                                       (lid ~loc [ "::" ])
                                                       (Some
                                                          (A.pexp_tuple ~loc
                                                             [
                                                               evar ~loc "mutation";
                                                               evar ~loc "acc";
                                                             ]));
                                                     evar ~loc "rest";
                                                   ]);
                                            A.case
                                              ~lhs:
                                                (A.ppat_construct ~loc
                                                   (lid ~loc [ "Error" ])
                                                   (Some (pvar ~loc "error")))
                                              ~guard:None
                                              ~rhs:
                                                (constr_arg ~loc [ "Error" ]
                                                   (evar ~loc "error"));
                                          ]);
                                 ])));
                  ]
                  (app ~loc (evar ~loc "loop")
                     [ list ~loc []; evar ~loc "rows" ])));
      ]
  in
  let create_many_result = result_list_fn "create_many_result" "create_record_result" in
  let create_many_values =
    A.pstr_value ~loc Nonrecursive
      [
        A.value_binding ~loc ~pat:(pvar ~loc "create_many_values")
          ~expr:
            (A.pexp_fun ~loc Nolabel None (pvar ~loc "rows")
               (app ~loc (ident ~loc [ "List"; "map" ])
                  [ evar ~loc "create_values"; evar ~loc "rows" ]));
      ]
  in
  let create_many_values_result =
    result_list_fn "create_many_values_result" "create_values_result"
  in
  let mutation_pipe_helpers =
    A.pstr_value ~loc Nonrecursive
      [
        A.value_binding ~loc ~pat:(pvar ~loc "set")
          ~expr:
            (A.pexp_fun ~loc Nolabel None (pvar ~loc "field")
               (A.pexp_fun ~loc Nolabel None (pvar ~loc "mutation")
                  (app ~loc (ident ~loc [ "Ent_ocaml"; "Mutation"; "set" ])
                     [ evar ~loc "field"; evar ~loc "mutation" ])));
        A.value_binding ~loc ~pat:(pvar ~loc "set_all")
          ~expr:
            (A.pexp_fun ~loc Nolabel None (pvar ~loc "fields")
               (A.pexp_fun ~loc Nolabel None (pvar ~loc "mutation")
                  (app ~loc (ident ~loc [ "Ent_ocaml"; "Mutation"; "set_all" ])
                     [ evar ~loc "fields"; evar ~loc "mutation" ])));
        A.value_binding ~loc ~pat:(pvar ~loc "clear")
          ~expr:
            (A.pexp_fun ~loc Nolabel None (pvar ~loc "field")
               (A.pexp_fun ~loc Nolabel None (pvar ~loc "mutation")
                  (app ~loc (ident ~loc [ "Ent_ocaml"; "Mutation"; "clear" ])
                     [ evar ~loc "field"; evar ~loc "mutation" ])));
        A.value_binding ~loc ~pat:(pvar ~loc "add")
          ~expr:
            (A.pexp_fun ~loc Nolabel None (pvar ~loc "field")
               (A.pexp_fun ~loc Nolabel None (pvar ~loc "mutation")
                  (app ~loc (ident ~loc [ "Ent_ocaml"; "Mutation"; "add" ])
                     [ evar ~loc "field"; evar ~loc "mutation" ])));
        A.value_binding ~loc ~pat:(pvar ~loc "on_insert")
          ~expr:
            (A.pexp_fun ~loc Nolabel None (pvar ~loc "field")
               (A.pexp_fun ~loc Nolabel None (pvar ~loc "mutation")
                  (app ~loc
                     (ident ~loc [ "Ent_ocaml"; "Mutation"; "on_insert" ])
                     [ evar ~loc "field"; evar ~loc "mutation" ])));
        A.value_binding ~loc ~pat:(pvar ~loc "on_insert_all")
          ~expr:
            (A.pexp_fun ~loc Nolabel None (pvar ~loc "fields")
               (A.pexp_fun ~loc Nolabel None (pvar ~loc "mutation")
                  (app ~loc
                     (ident ~loc [ "Ent_ocaml"; "Mutation"; "on_insert_all" ])
                     [ evar ~loc "fields"; evar ~loc "mutation" ])));
      ]
  in
  let update_fn name op =
    let body =
      mutation_record ~op ~predicates:(evar ~loc "where")
        ~set:(evar ~loc "set")
        ~clear:(evar ~loc "clear")
        ~add:(evar ~loc "add") ~on_insert:(list ~loc [])
    in
    A.pstr_value ~loc Nonrecursive
      [
        A.value_binding ~loc ~pat:(pvar ~loc name)
          ~expr:
            (A.pexp_fun ~loc (Optional "where") (Some (list ~loc []))
               (pvar ~loc "where")
               (A.pexp_fun ~loc (Optional "set") (Some (list ~loc []))
                  (pvar ~loc "set")
                  (A.pexp_fun ~loc (Optional "clear") (Some (list ~loc []))
                     (pvar ~loc "clear")
                     (A.pexp_fun ~loc (Optional "add") (Some (list ~loc []))
                        (pvar ~loc "add")
                        (A.pexp_fun ~loc Nolabel None (unit_pat ~loc)
                           (apply_update_default_bindings ~loc fields body))))));
      ]
  in
  let update_result_fn name op =
    let body =
      mutation_record ~op ~predicates:(evar ~loc "where")
        ~set:(evar ~loc "set")
        ~clear:(evar ~loc "clear")
        ~add:(evar ~loc "add") ~on_insert:(list ~loc [])
      |> constr_arg ~loc [ "Ok" ]
    in
    A.pstr_value ~loc Nonrecursive
      [
        A.value_binding ~loc ~pat:(pvar ~loc name)
          ~expr:
            (A.pexp_fun ~loc (Optional "where") (Some (list ~loc []))
               (pvar ~loc "where")
               (A.pexp_fun ~loc (Optional "set") (Some (list ~loc []))
                  (pvar ~loc "set")
                  (A.pexp_fun ~loc (Optional "clear") (Some (list ~loc []))
                     (pvar ~loc "clear")
                     (A.pexp_fun ~loc (Optional "add") (Some (list ~loc []))
                        (pvar ~loc "add")
                        (A.pexp_fun ~loc Nolabel None (unit_pat ~loc)
                           (apply_update_default_bindings ~loc fields
                              (apply_update_default_result_bindings ~loc fields
                                 body)))))));
      ]
  in
  let update_query_fn name op =
    let body =
      mutation_record ~op
        ~predicates:
          (A.pexp_field ~loc (evar ~loc "query")
             (lid ~loc [ "Ent_ocaml"; "predicates" ]))
        ~set:(evar ~loc "set")
        ~clear:(evar ~loc "clear")
        ~add:(evar ~loc "add") ~on_insert:(list ~loc [])
    in
    A.pstr_value ~loc Nonrecursive
      [
        A.value_binding ~loc ~pat:(pvar ~loc name)
          ~expr:
            (A.pexp_fun ~loc (Optional "set") (Some (list ~loc []))
               (pvar ~loc "set")
               (A.pexp_fun ~loc (Optional "clear") (Some (list ~loc []))
                  (pvar ~loc "clear")
                  (A.pexp_fun ~loc (Optional "add") (Some (list ~loc []))
                     (pvar ~loc "add")
                     (A.pexp_fun ~loc Nolabel None query_pat
                        (apply_update_default_bindings ~loc fields body)))));
      ]
  in
  let update_query_result_fn name op =
    let body =
      mutation_record ~op
        ~predicates:
          (A.pexp_field ~loc (evar ~loc "query")
             (lid ~loc [ "Ent_ocaml"; "predicates" ]))
        ~set:(evar ~loc "set")
        ~clear:(evar ~loc "clear")
        ~add:(evar ~loc "add") ~on_insert:(list ~loc [])
      |> constr_arg ~loc [ "Ok" ]
    in
    A.pstr_value ~loc Nonrecursive
      [
        A.value_binding ~loc ~pat:(pvar ~loc name)
          ~expr:
            (A.pexp_fun ~loc (Optional "set") (Some (list ~loc []))
               (pvar ~loc "set")
               (A.pexp_fun ~loc (Optional "clear") (Some (list ~loc []))
                  (pvar ~loc "clear")
                  (A.pexp_fun ~loc (Optional "add") (Some (list ~loc []))
                     (pvar ~loc "add")
                     (A.pexp_fun ~loc Nolabel None query_pat
                        (apply_update_default_bindings ~loc fields
                           (apply_update_default_result_bindings ~loc fields body))))));
      ]
  in
  let upsert_query_fn =
    let body =
      mutation_record ~op:"Upsert_one"
        ~predicates:
          (A.pexp_field ~loc (evar ~loc "query")
             (lid ~loc [ "Ent_ocaml"; "predicates" ]))
        ~set:(list ~loc []) ~clear:(list ~loc []) ~add:(list ~loc [])
        ~on_insert:(list ~loc [])
    in
    A.pstr_value ~loc Nonrecursive
      [
        A.value_binding ~loc ~pat:(pvar ~loc "upsert_where")
          ~expr:(A.pexp_fun ~loc Nolabel None query_pat body);
      ]
  in
  let upsert_fn =
    A.pstr_value ~loc Nonrecursive
      [
        A.value_binding ~loc ~pat:(pvar ~loc "upsert_one")
          ~expr:
            (A.pexp_fun ~loc (Optional "where") (Some (list ~loc []))
               (pvar ~loc "where")
               (A.pexp_fun ~loc Nolabel None (unit_pat ~loc)
                  (mutation_record ~op:"Upsert_one"
                     ~predicates:(evar ~loc "where")
                     ~set:(list ~loc []) ~clear:(list ~loc [])
                     ~add:(list ~loc []) ~on_insert:(list ~loc []))));
      ]
  in
  let delete_fn name op =
    A.pstr_value ~loc Nonrecursive
      [
        A.value_binding ~loc ~pat:(pvar ~loc name)
          ~expr:
            (A.pexp_fun ~loc (Optional "where") (Some (list ~loc []))
               (pvar ~loc "where")
               (A.pexp_fun ~loc Nolabel None (unit_pat ~loc)
                  (mutation_record ~op ~predicates:(evar ~loc "where")
                     ~set:(list ~loc []) ~clear:(list ~loc [])
                     ~add:(list ~loc []) ~on_insert:(list ~loc []))));
      ]
  in
  let delete_query_fn name op =
    A.pstr_value ~loc Nonrecursive
      [
        A.value_binding ~loc ~pat:(pvar ~loc name)
          ~expr:
            (A.pexp_fun ~loc Nolabel None query_pat
               (mutation_record ~op
                  ~predicates:
                    (A.pexp_field ~loc (evar ~loc "query")
                       (lid ~loc [ "Ent_ocaml"; "predicates" ]))
                  ~set:(list ~loc []) ~clear:(list ~loc [])
                  ~add:(list ~loc []) ~on_insert:(list ~loc [])));
      ]
  in
  let id_helper_items =
    match
      List.find_opt
        (fun field ->
          field.pld_name.txt = "id" && Option.is_some (value_constructor field))
        fields
    with
    | None -> []
    | Some _ ->
        let by_id_expr =
          app ~loc
            (app ~loc (evar ~loc "where")
               [
                 app ~loc (evar ~loc "id_eq") [ evar ~loc "value" ];
               ])
            [ app ~loc (evar ~loc "query") [ unit ~loc ] ]
        in
        [
          A.pstr_value ~loc Nonrecursive
            [
              A.value_binding ~loc ~pat:(pvar ~loc "by_id")
                ~expr:
                  (A.pexp_fun ~loc Nolabel None (pvar ~loc "value")
                     by_id_expr);
            ];
          A.pstr_value ~loc Nonrecursive
            [
              A.value_binding ~loc ~pat:(pvar ~loc "update_id")
                ~expr:
                  (A.pexp_fun ~loc Nolabel None (pvar ~loc "value")
                     (app ~loc (evar ~loc "update_one_where")
                        [ app ~loc (evar ~loc "by_id") [ evar ~loc "value" ] ]));
            ];
          A.pstr_value ~loc Nonrecursive
            [
              A.value_binding ~loc ~pat:(pvar ~loc "update_id_result")
                ~expr:
                  (A.pexp_fun ~loc Nolabel None (pvar ~loc "value")
                     (app ~loc (evar ~loc "update_one_where_result")
                        [ app ~loc (evar ~loc "by_id") [ evar ~loc "value" ] ]));
            ];
          A.pstr_value ~loc Nonrecursive
            [
              A.value_binding ~loc ~pat:(pvar ~loc "delete_id")
                ~expr:
                  (A.pexp_fun ~loc Nolabel None (pvar ~loc "value")
                     (app ~loc (evar ~loc "delete_one_where")
                        [ app ~loc (evar ~loc "by_id") [ evar ~loc "value" ] ]));
            ];
        ]
  in
  let store_module =
    let backend_type =
      A.pmty_ident ~loc (lid ~loc [ "Ent_ocaml"; "STORE_BACKEND" ])
    in
    let backend_apply name args =
      A.pexp_apply ~loc
        (A.pexp_ident ~loc (lid ~loc [ "Backend"; name ]))
        args
    in
    let value_fun name body =
      A.pstr_value ~loc Nonrecursive
        [ A.value_binding ~loc ~pat:(pvar ~loc name) ~expr:body ]
    in
    let typed_pat name path =
      A.ppat_constraint ~loc (pvar ~loc name)
        (A.ptyp_constr ~loc (lid ~loc path) [])
    in
    let edge_query_pat = typed_pat "edge_query" [ "Ent_ocaml"; "edge_query" ] in
    let aggregate_pat = typed_pat "aggregate" [ "Ent_ocaml"; "aggregate" ] in
    let aggregate_scan_pat =
      typed_pat "scan" [ "Ent_ocaml"; "aggregate_scan" ]
    in
    let group_aggregate_pat =
      typed_pat "group" [ "Ent_ocaml"; "group_aggregate" ]
    in
    let error_case =
      A.case
        ~lhs:
          (A.ppat_construct ~loc (lid ~loc [ "Error" ])
             (Some (pvar ~loc "error")))
        ~guard:None
        ~rhs:(constr_arg ~loc [ "Error" ] (evar ~loc "error"))
    in
    let ok_case body =
      A.case
        ~lhs:
          (A.ppat_construct ~loc (lid ~loc [ "Ok" ])
             (Some (A.ppat_construct ~loc (lid ~loc [ "()" ]) None)))
        ~guard:None ~rhs:body
    in
    let privacy_check kind checked =
      let evaluator =
        match kind with
        | `Query -> [ "Ent_ocaml"; "Privacy"; "evaluate_query" ]
        | `Mutation -> [ "Ent_ocaml"; "Privacy"; "evaluate_mutation" ]
        | `Mutations -> [ "Ent_ocaml"; "Privacy"; "evaluate_mutations" ]
      in
      let rules =
        match kind with
        | `Query -> [ "Policy"; "query_rules" ]
        | `Mutation | `Mutations -> [ "Policy"; "mutation_rules" ]
      in
      A.pexp_apply ~loc
        (ident ~loc evaluator)
        [
          (Nolabel, evar ~loc "ctx");
          (Nolabel, ident ~loc rules);
          (Nolabel, checked);
        ]
    in
    let guarded kind checked body =
      A.pexp_match ~loc (privacy_check kind checked) [ error_case; ok_case body ]
    in
    let policy_type =
      let backend_ctx =
        A.ptyp_constr ~loc (lid ~loc [ "Backend"; "ctx" ]) []
      in
      let query_rule_typ =
        A.ptyp_constr ~loc
          (lid ~loc [ "Ent_ocaml"; "query_rule" ])
          [ backend_ctx ]
      in
      let mutation_rule_typ =
        A.ptyp_constr ~loc
          (lid ~loc [ "Ent_ocaml"; "mutation_rule" ])
          [ backend_ctx ]
      in
      let value_sig name type_ =
        A.psig_value ~loc
          (A.value_description ~loc ~name:{ loc; txt = name } ~type_ ~prim:[])
      in
      let list_typ typ = A.ptyp_constr ~loc (lid ~loc [ "list" ]) [ typ ] in
      A.pmty_signature ~loc
        [
          value_sig "query_rules" (list_typ query_rule_typ);
          value_sig "mutation_rules" (list_typ mutation_rule_typ);
        ]
    in
    let hooks_type =
      let backend_ctx =
        A.ptyp_constr ~loc (lid ~loc [ "Backend"; "ctx" ]) []
      in
      let mutation_hook_typ =
        A.ptyp_constr ~loc
          (lid ~loc [ "Ent_ocaml"; "mutation_hook" ])
          [ backend_ctx ]
      in
      let value_sig name type_ =
        A.psig_value ~loc
          (A.value_description ~loc ~name:{ loc; txt = name } ~type_ ~prim:[])
      in
      let list_typ typ = A.ptyp_constr ~loc (lid ~loc [ "list" ]) [ typ ] in
      A.pmty_signature ~loc
        [ value_sig "mutation_hooks" (list_typ mutation_hook_typ) ]
    in
    let interceptors_type =
      let backend_ctx =
        A.ptyp_constr ~loc (lid ~loc [ "Backend"; "ctx" ]) []
      in
      let query_interceptor_typ =
        A.ptyp_constr ~loc
          (lid ~loc [ "Ent_ocaml"; "query_interceptor" ])
          [ backend_ctx ]
      in
      let value_sig name type_ =
        A.psig_value ~loc
          (A.value_description ~loc ~name:{ loc; txt = name } ~type_ ~prim:[])
      in
      let list_typ typ = A.ptyp_constr ~loc (lid ~loc [ "list" ]) [ typ ] in
      A.pmty_signature ~loc
        [ value_sig "query_interceptors" (list_typ query_interceptor_typ) ]
    in
    let with_policy_module =
      let structure =
        [
          value_fun "all"
            (A.pexp_fun ~loc Nolabel None (pvar ~loc "ctx")
               (A.pexp_fun ~loc (Labelled "decode") None (pvar ~loc "decode")
                  (A.pexp_fun ~loc Nolabel None query_pat
                     (guarded `Query (evar ~loc "query")
                        (backend_apply "find_as"
                           [
                             (Nolabel, evar ~loc "ctx");
                             (Nolabel, evar ~loc "query");
                             (Labelled "decode", evar ~loc "decode");
                           ])))));
          value_fun "one"
            (A.pexp_fun ~loc Nolabel None (pvar ~loc "ctx")
               (A.pexp_fun ~loc (Labelled "decode") None (pvar ~loc "decode")
                  (A.pexp_fun ~loc Nolabel None query_pat
                     (guarded `Query (evar ~loc "query")
                        (backend_apply "find_one_as"
                           [
                             (Nolabel, evar ~loc "ctx");
                             (Nolabel, evar ~loc "query");
                             (Labelled "decode", evar ~loc "decode");
                           ])))));
          value_fun "values"
            (A.pexp_fun ~loc Nolabel None (pvar ~loc "ctx")
               (A.pexp_fun ~loc Nolabel None query_pat
                  (guarded `Query (evar ~loc "query")
                     (backend_apply "values"
                        [
                          (Nolabel, evar ~loc "ctx");
                          (Nolabel, evar ~loc "query");
                        ]))));
          value_fun "value"
            (A.pexp_fun ~loc Nolabel None (pvar ~loc "ctx")
               (A.pexp_fun ~loc Nolabel None query_pat
                  (guarded `Query (evar ~loc "query")
                     (backend_apply "value"
                        [
                          (Nolabel, evar ~loc "ctx");
                          (Nolabel, evar ~loc "query");
                        ]))));
          value_fun "traverse"
            (A.pexp_fun ~loc Nolabel None (pvar ~loc "ctx")
               (A.pexp_fun ~loc (Labelled "decode") None (pvar ~loc "decode")
                  (A.pexp_fun ~loc Nolabel None edge_query_pat
                     (guarded `Query
                        (A.pexp_field ~loc (evar ~loc "edge_query")
                           (lid ~loc [ "Ent_ocaml"; "source" ]))
                        (backend_apply "traverse_as"
                           [
                             (Nolabel, evar ~loc "ctx");
                             (Nolabel, evar ~loc "edge_query");
                             (Labelled "decode", evar ~loc "decode");
                           ])))));
          value_fun "load_edge"
            (A.pexp_fun ~loc Nolabel None (pvar ~loc "ctx")
               (A.pexp_fun ~loc (Labelled "decode_source") None
                  (pvar ~loc "decode_source")
                  (A.pexp_fun ~loc (Labelled "decode_target") None
                     (pvar ~loc "decode_target")
                     (A.pexp_fun ~loc Nolabel None edge_query_pat
                        (guarded `Query
                           (A.pexp_field ~loc (evar ~loc "edge_query")
                              (lid ~loc [ "Ent_ocaml"; "source" ]))
                           (backend_apply "load_edge_as"
                              [
                                (Nolabel, evar ~loc "ctx");
                                (Nolabel, evar ~loc "edge_query");
                                (Labelled "decode_source", evar ~loc "decode_source");
                                (Labelled "decode_target", evar ~loc "decode_target");
                              ]))))));
          value_fun "count"
            (A.pexp_fun ~loc Nolabel None (pvar ~loc "ctx")
               (A.pexp_fun ~loc Nolabel None query_pat
                  (guarded `Query (evar ~loc "query")
                     (backend_apply "count"
                        [
                          (Nolabel, evar ~loc "ctx");
                          (Nolabel, evar ~loc "query");
                        ]))));
          value_fun "aggregate"
            (A.pexp_fun ~loc Nolabel None (pvar ~loc "ctx")
               (A.pexp_fun ~loc Nolabel None aggregate_pat
                  (guarded `Query
                     (A.pexp_field ~loc (evar ~loc "aggregate")
                        (lid ~loc [ "Ent_ocaml"; "query" ]))
                     (backend_apply "aggregate"
                        [
                          (Nolabel, evar ~loc "ctx");
                          (Nolabel, evar ~loc "aggregate");
                        ]))));
          value_fun "aggregate_scan"
            (A.pexp_fun ~loc Nolabel None (pvar ~loc "ctx")
               (A.pexp_fun ~loc Nolabel None aggregate_scan_pat
                  (guarded `Query
                     (A.pexp_field ~loc (evar ~loc "scan")
                        (lid ~loc [ "Ent_ocaml"; "query" ]))
                     (backend_apply "aggregate_scan"
                        [
                          (Nolabel, evar ~loc "ctx");
                          (Nolabel, evar ~loc "scan");
                        ]))));
          value_fun "group"
            (A.pexp_fun ~loc Nolabel None (pvar ~loc "ctx")
               (A.pexp_fun ~loc Nolabel None group_aggregate_pat
                  (guarded `Query
                     (A.pexp_field ~loc
                        (A.pexp_field ~loc (evar ~loc "group")
                           (lid ~loc [ "Ent_ocaml"; "aggregate" ]))
                        (lid ~loc [ "Ent_ocaml"; "query" ]))
                     (backend_apply "group"
                        [
                          (Nolabel, evar ~loc "ctx");
                          (Nolabel, evar ~loc "group");
                        ]))));
          value_fun "insert"
            (A.pexp_fun ~loc Nolabel None (pvar ~loc "ctx")
               (A.pexp_fun ~loc Nolabel None (pvar ~loc "mutation")
                  (guarded `Mutation (evar ~loc "mutation")
                     (backend_apply "insert_values"
                        [
                          (Nolabel, evar ~loc "ctx");
                          (Nolabel, evar ~loc "mutation");
                        ]))));
          value_fun "insert_many"
            (A.pexp_fun ~loc (Optional "ordered") None (pvar ~loc "ordered")
               (A.pexp_fun ~loc Nolabel None (pvar ~loc "ctx")
                  (A.pexp_fun ~loc Nolabel None (pvar ~loc "mutations")
                     (guarded `Mutations (evar ~loc "mutations")
                        (backend_apply "insert_many_values"
                           [
                             (Optional "ordered", evar ~loc "ordered");
                             (Nolabel, evar ~loc "ctx");
                             (Nolabel, evar ~loc "mutations");
                           ])))));
          value_fun "update_one"
            (A.pexp_fun ~loc Nolabel None (pvar ~loc "ctx")
               (A.pexp_fun ~loc Nolabel None (pvar ~loc "mutation")
                  (guarded `Mutation (evar ~loc "mutation")
                     (backend_apply "update_one"
                        [
                          (Nolabel, evar ~loc "ctx");
                          (Nolabel, evar ~loc "mutation");
                        ]))));
          value_fun "update"
            (A.pexp_fun ~loc Nolabel None (pvar ~loc "ctx")
               (A.pexp_fun ~loc Nolabel None (pvar ~loc "mutation")
                  (guarded `Mutation (evar ~loc "mutation")
                     (backend_apply "update"
                        [
                          (Nolabel, evar ~loc "ctx");
                          (Nolabel, evar ~loc "mutation");
                        ]))));
          value_fun "upsert_one"
            (A.pexp_fun ~loc Nolabel None (pvar ~loc "ctx")
               (A.pexp_fun ~loc Nolabel None (pvar ~loc "mutation")
                  (guarded `Mutation (evar ~loc "mutation")
                     (backend_apply "upsert_one"
                        [
                          (Nolabel, evar ~loc "ctx");
                          (Nolabel, evar ~loc "mutation");
                        ]))));
          value_fun "delete"
            (A.pexp_fun ~loc Nolabel None (pvar ~loc "ctx")
               (A.pexp_fun ~loc Nolabel None (pvar ~loc "mutation")
                  (guarded `Mutation (evar ~loc "mutation")
                     (backend_apply "delete"
                        [
                          (Nolabel, evar ~loc "ctx");
                          (Nolabel, evar ~loc "mutation");
                        ]))));
        ]
      in
      A.pstr_module ~loc
        (A.module_binding ~loc ~name:{ loc; txt = Some "With_policy" }
           ~expr:
             (A.pmod_functor ~loc
                (Named ({ loc; txt = Some "Policy" }, policy_type))
                (A.pmod_structure ~loc structure)))
    in
    let with_hooks_module =
      let hook_run backend_name =
        A.pexp_apply ~loc
          (ident ~loc [ "Ent_ocaml"; "Hook"; "run_mutation" ])
          [
            (Nolabel, ident ~loc [ "Hooks"; "mutation_hooks" ]);
            (Nolabel, ident ~loc [ "Backend"; backend_name ]);
            (Nolabel, evar ~loc "ctx");
            (Nolabel, evar ~loc "mutation");
          ]
      in
      let hook_many body =
        A.pexp_match ~loc
          (A.pexp_apply ~loc
             (ident ~loc [ "Ent_ocaml"; "Hook"; "run_mutations" ])
             [
               (Nolabel, ident ~loc [ "Hooks"; "mutation_hooks" ]);
               (Nolabel, evar ~loc "ctx");
               (Nolabel, evar ~loc "mutations");
             ])
          [
            error_case;
            A.case
              ~lhs:
                (A.ppat_construct ~loc (lid ~loc [ "Ok" ])
                   (Some (pvar ~loc "mutations")))
              ~guard:None ~rhs:body;
          ]
      in
      let structure =
        [
          value_fun "all"
            (A.pexp_fun ~loc Nolabel None (pvar ~loc "ctx")
               (A.pexp_fun ~loc (Labelled "decode") None (pvar ~loc "decode")
                  (A.pexp_fun ~loc Nolabel None query_pat
                     (backend_apply "find_as"
                        [
                          (Nolabel, evar ~loc "ctx");
                          (Nolabel, evar ~loc "query");
                          (Labelled "decode", evar ~loc "decode");
                        ]))));
          value_fun "one"
            (A.pexp_fun ~loc Nolabel None (pvar ~loc "ctx")
               (A.pexp_fun ~loc (Labelled "decode") None (pvar ~loc "decode")
                  (A.pexp_fun ~loc Nolabel None query_pat
                     (backend_apply "find_one_as"
                        [
                          (Nolabel, evar ~loc "ctx");
                          (Nolabel, evar ~loc "query");
                          (Labelled "decode", evar ~loc "decode");
                        ]))));
          value_fun "values"
            (A.pexp_fun ~loc Nolabel None (pvar ~loc "ctx")
               (A.pexp_fun ~loc Nolabel None query_pat
                  (backend_apply "values"
                     [
                       (Nolabel, evar ~loc "ctx");
                       (Nolabel, evar ~loc "query");
                     ])));
          value_fun "value"
            (A.pexp_fun ~loc Nolabel None (pvar ~loc "ctx")
               (A.pexp_fun ~loc Nolabel None query_pat
                  (backend_apply "value"
                     [
                       (Nolabel, evar ~loc "ctx");
                       (Nolabel, evar ~loc "query");
                     ])));
          value_fun "traverse"
            (A.pexp_fun ~loc Nolabel None (pvar ~loc "ctx")
               (A.pexp_fun ~loc (Labelled "decode") None (pvar ~loc "decode")
                  (A.pexp_fun ~loc Nolabel None edge_query_pat
                     (backend_apply "traverse_as"
                        [
                          (Nolabel, evar ~loc "ctx");
                          (Nolabel, evar ~loc "edge_query");
                          (Labelled "decode", evar ~loc "decode");
                        ]))));
          value_fun "load_edge"
            (A.pexp_fun ~loc Nolabel None (pvar ~loc "ctx")
               (A.pexp_fun ~loc (Labelled "decode_source") None
                  (pvar ~loc "decode_source")
                  (A.pexp_fun ~loc (Labelled "decode_target") None
                     (pvar ~loc "decode_target")
                     (A.pexp_fun ~loc Nolabel None edge_query_pat
                        (backend_apply "load_edge_as"
                           [
                             (Nolabel, evar ~loc "ctx");
                             (Nolabel, evar ~loc "edge_query");
                             (Labelled "decode_source", evar ~loc "decode_source");
                             (Labelled "decode_target", evar ~loc "decode_target");
                           ])))));
          value_fun "count"
            (A.pexp_fun ~loc Nolabel None (pvar ~loc "ctx")
               (A.pexp_fun ~loc Nolabel None query_pat
                  (backend_apply "count"
                     [
                       (Nolabel, evar ~loc "ctx");
                       (Nolabel, evar ~loc "query");
                     ])));
          value_fun "aggregate"
            (A.pexp_fun ~loc Nolabel None (pvar ~loc "ctx")
               (A.pexp_fun ~loc Nolabel None aggregate_pat
                  (backend_apply "aggregate"
                     [
                       (Nolabel, evar ~loc "ctx");
                       (Nolabel, evar ~loc "aggregate");
                     ])));
          value_fun "aggregate_scan"
            (A.pexp_fun ~loc Nolabel None (pvar ~loc "ctx")
               (A.pexp_fun ~loc Nolabel None aggregate_scan_pat
                  (backend_apply "aggregate_scan"
                     [
                       (Nolabel, evar ~loc "ctx");
                       (Nolabel, evar ~loc "scan");
                     ])));
          value_fun "group"
            (A.pexp_fun ~loc Nolabel None (pvar ~loc "ctx")
               (A.pexp_fun ~loc Nolabel None group_aggregate_pat
                  (backend_apply "group"
                     [
                       (Nolabel, evar ~loc "ctx");
                       (Nolabel, evar ~loc "group");
                     ])));
          value_fun "insert"
            (A.pexp_fun ~loc Nolabel None (pvar ~loc "ctx")
               (A.pexp_fun ~loc Nolabel None (pvar ~loc "mutation")
                  (hook_run "insert_values")));
          value_fun "insert_many"
            (A.pexp_fun ~loc (Optional "ordered") None (pvar ~loc "ordered")
               (A.pexp_fun ~loc Nolabel None (pvar ~loc "ctx")
                  (A.pexp_fun ~loc Nolabel None (pvar ~loc "mutations")
                     (hook_many
                        (backend_apply "insert_many_values"
                           [
                             (Optional "ordered", evar ~loc "ordered");
                             (Nolabel, evar ~loc "ctx");
                             (Nolabel, evar ~loc "mutations");
                           ])))));
          value_fun "update_one"
            (A.pexp_fun ~loc Nolabel None (pvar ~loc "ctx")
               (A.pexp_fun ~loc Nolabel None (pvar ~loc "mutation")
                  (hook_run "update_one")));
          value_fun "update"
            (A.pexp_fun ~loc Nolabel None (pvar ~loc "ctx")
               (A.pexp_fun ~loc Nolabel None (pvar ~loc "mutation")
                  (hook_run "update")));
          value_fun "upsert_one"
            (A.pexp_fun ~loc Nolabel None (pvar ~loc "ctx")
               (A.pexp_fun ~loc Nolabel None (pvar ~loc "mutation")
                  (hook_run "upsert_one")));
          value_fun "delete"
            (A.pexp_fun ~loc Nolabel None (pvar ~loc "ctx")
               (A.pexp_fun ~loc Nolabel None (pvar ~loc "mutation")
                  (hook_run "delete")));
        ]
      in
      A.pstr_module ~loc
        (A.module_binding ~loc ~name:{ loc; txt = Some "With_hooks" }
           ~expr:
             (A.pmod_functor ~loc
                (Named ({ loc; txt = Some "Hooks" }, hooks_type))
                (A.pmod_structure ~loc structure)))
    in
    let with_interceptors_module =
      let run_query query_expr body =
        let next =
          A.pexp_fun ~loc Nolabel None (pvar ~loc "ctx")
            (A.pexp_fun ~loc Nolabel None query_pat body)
        in
        A.pexp_apply ~loc
          (ident ~loc [ "Ent_ocaml"; "Interceptor"; "run_query" ])
          [
            (Nolabel, ident ~loc [ "Interceptors"; "query_interceptors" ]);
            (Nolabel, next);
            (Nolabel, evar ~loc "ctx");
            (Nolabel, query_expr);
          ]
      in
      let aggregate_with_query =
        A.pexp_record ~loc
          [ (lid ~loc [ "Ent_ocaml"; "query" ], evar ~loc "query") ]
          (Some (evar ~loc "aggregate"))
      in
      let scan_with_query =
        A.pexp_record ~loc
          [ (lid ~loc [ "Ent_ocaml"; "query" ], evar ~loc "query") ]
          (Some (evar ~loc "scan"))
      in
      let edge_query_with_source =
        A.pexp_record ~loc
          [ (lid ~loc [ "Ent_ocaml"; "source" ], evar ~loc "query") ]
          (Some (evar ~loc "edge_query"))
      in
      let group_with_query =
        let aggregate =
          A.pexp_record ~loc
            [ (lid ~loc [ "Ent_ocaml"; "query" ], evar ~loc "query") ]
            (Some
               (A.pexp_field ~loc (evar ~loc "group")
                  (lid ~loc [ "Ent_ocaml"; "aggregate" ])))
        in
        A.pexp_record ~loc
          [ (lid ~loc [ "Ent_ocaml"; "aggregate" ], aggregate) ]
          (Some (evar ~loc "group"))
      in
      let structure =
        [
          value_fun "all"
            (A.pexp_fun ~loc Nolabel None (pvar ~loc "ctx")
               (A.pexp_fun ~loc (Labelled "decode") None (pvar ~loc "decode")
                  (A.pexp_fun ~loc Nolabel None query_pat
                     (run_query (evar ~loc "query")
                        (backend_apply "find_as"
                           [
                             (Nolabel, evar ~loc "ctx");
                             (Nolabel, evar ~loc "query");
                             (Labelled "decode", evar ~loc "decode");
                           ])))));
          value_fun "one"
            (A.pexp_fun ~loc Nolabel None (pvar ~loc "ctx")
               (A.pexp_fun ~loc (Labelled "decode") None (pvar ~loc "decode")
                  (A.pexp_fun ~loc Nolabel None query_pat
                     (run_query (evar ~loc "query")
                        (backend_apply "find_one_as"
                           [
                             (Nolabel, evar ~loc "ctx");
                             (Nolabel, evar ~loc "query");
                             (Labelled "decode", evar ~loc "decode");
                           ])))));
          value_fun "values"
            (A.pexp_fun ~loc Nolabel None (pvar ~loc "ctx")
               (A.pexp_fun ~loc Nolabel None query_pat
                  (run_query (evar ~loc "query")
                     (backend_apply "values"
                        [
                          (Nolabel, evar ~loc "ctx");
                          (Nolabel, evar ~loc "query");
                        ]))));
          value_fun "value"
            (A.pexp_fun ~loc Nolabel None (pvar ~loc "ctx")
               (A.pexp_fun ~loc Nolabel None query_pat
                  (run_query (evar ~loc "query")
                     (backend_apply "value"
                        [
                          (Nolabel, evar ~loc "ctx");
                          (Nolabel, evar ~loc "query");
                        ]))));
          value_fun "traverse"
            (A.pexp_fun ~loc Nolabel None (pvar ~loc "ctx")
               (A.pexp_fun ~loc (Labelled "decode") None (pvar ~loc "decode")
                  (A.pexp_fun ~loc Nolabel None edge_query_pat
                     (run_query
                        (A.pexp_field ~loc (evar ~loc "edge_query")
                           (lid ~loc [ "Ent_ocaml"; "source" ]))
                        (backend_apply "traverse_as"
                           [
                             (Nolabel, evar ~loc "ctx");
                             (Nolabel, edge_query_with_source);
                             (Labelled "decode", evar ~loc "decode");
                           ])))));
          value_fun "load_edge"
            (A.pexp_fun ~loc Nolabel None (pvar ~loc "ctx")
               (A.pexp_fun ~loc (Labelled "decode_source") None
                  (pvar ~loc "decode_source")
                  (A.pexp_fun ~loc (Labelled "decode_target") None
                     (pvar ~loc "decode_target")
                     (A.pexp_fun ~loc Nolabel None edge_query_pat
                        (run_query
                           (A.pexp_field ~loc (evar ~loc "edge_query")
                              (lid ~loc [ "Ent_ocaml"; "source" ]))
                           (backend_apply "load_edge_as"
                              [
                                (Nolabel, evar ~loc "ctx");
                                (Nolabel, edge_query_with_source);
                                (Labelled "decode_source", evar ~loc "decode_source");
                                (Labelled "decode_target", evar ~loc "decode_target");
                              ]))))));
          value_fun "count"
            (A.pexp_fun ~loc Nolabel None (pvar ~loc "ctx")
               (A.pexp_fun ~loc Nolabel None query_pat
                  (run_query (evar ~loc "query")
                     (backend_apply "count"
                        [
                          (Nolabel, evar ~loc "ctx");
                          (Nolabel, evar ~loc "query");
                        ]))));
          value_fun "aggregate"
            (A.pexp_fun ~loc Nolabel None (pvar ~loc "ctx")
               (A.pexp_fun ~loc Nolabel None aggregate_pat
                  (run_query
                     (A.pexp_field ~loc (evar ~loc "aggregate")
                        (lid ~loc [ "Ent_ocaml"; "query" ]))
                     (backend_apply "aggregate"
                        [
                          (Nolabel, evar ~loc "ctx");
                          (Nolabel, aggregate_with_query);
                        ]))));
          value_fun "aggregate_scan"
            (A.pexp_fun ~loc Nolabel None (pvar ~loc "ctx")
               (A.pexp_fun ~loc Nolabel None aggregate_scan_pat
                  (run_query
                     (A.pexp_field ~loc (evar ~loc "scan")
                        (lid ~loc [ "Ent_ocaml"; "query" ]))
                     (backend_apply "aggregate_scan"
                        [
                          (Nolabel, evar ~loc "ctx");
                          (Nolabel, scan_with_query);
                        ]))));
          value_fun "group"
            (A.pexp_fun ~loc Nolabel None (pvar ~loc "ctx")
               (A.pexp_fun ~loc Nolabel None group_aggregate_pat
                  (run_query
                     (A.pexp_field ~loc
                        (A.pexp_field ~loc (evar ~loc "group")
                           (lid ~loc [ "Ent_ocaml"; "aggregate" ]))
                        (lid ~loc [ "Ent_ocaml"; "query" ]))
                     (backend_apply "group"
                        [
                          (Nolabel, evar ~loc "ctx");
                          (Nolabel, group_with_query);
                        ]))));
          value_fun "insert"
            (A.pexp_fun ~loc Nolabel None (pvar ~loc "ctx")
               (A.pexp_fun ~loc Nolabel None (pvar ~loc "mutation")
                  (backend_apply "insert_values"
                     [
                       (Nolabel, evar ~loc "ctx");
                       (Nolabel, evar ~loc "mutation");
                     ])));
          value_fun "insert_many"
            (A.pexp_fun ~loc (Optional "ordered") None (pvar ~loc "ordered")
               (A.pexp_fun ~loc Nolabel None (pvar ~loc "ctx")
                  (A.pexp_fun ~loc Nolabel None (pvar ~loc "mutations")
                     (backend_apply "insert_many_values"
                        [
                          (Optional "ordered", evar ~loc "ordered");
                          (Nolabel, evar ~loc "ctx");
                          (Nolabel, evar ~loc "mutations");
                        ]))));
          value_fun "update_one"
            (A.pexp_fun ~loc Nolabel None (pvar ~loc "ctx")
               (A.pexp_fun ~loc Nolabel None (pvar ~loc "mutation")
                  (backend_apply "update_one"
                     [
                       (Nolabel, evar ~loc "ctx");
                       (Nolabel, evar ~loc "mutation");
                     ])));
          value_fun "update"
            (A.pexp_fun ~loc Nolabel None (pvar ~loc "ctx")
               (A.pexp_fun ~loc Nolabel None (pvar ~loc "mutation")
                  (backend_apply "update"
                     [
                       (Nolabel, evar ~loc "ctx");
                       (Nolabel, evar ~loc "mutation");
                     ])));
          value_fun "upsert_one"
            (A.pexp_fun ~loc Nolabel None (pvar ~loc "ctx")
               (A.pexp_fun ~loc Nolabel None (pvar ~loc "mutation")
                  (backend_apply "upsert_one"
                     [
                       (Nolabel, evar ~loc "ctx");
                       (Nolabel, evar ~loc "mutation");
                     ])));
          value_fun "delete"
            (A.pexp_fun ~loc Nolabel None (pvar ~loc "ctx")
               (A.pexp_fun ~loc Nolabel None (pvar ~loc "mutation")
                  (backend_apply "delete"
                     [
                       (Nolabel, evar ~loc "ctx");
                       (Nolabel, evar ~loc "mutation");
                     ])));
        ]
      in
      A.pstr_module ~loc
        (A.module_binding ~loc ~name:{ loc; txt = Some "With_interceptors" }
           ~expr:
             (A.pmod_functor ~loc
                (Named ({ loc; txt = Some "Interceptors" }, interceptors_type))
                (A.pmod_structure ~loc structure)))
    in
    let schema_module name functor_name bindings =
      A.pstr_module ~loc
        (A.module_binding ~loc ~name:{ loc; txt = Some name }
           ~expr:
             (A.pmod_apply ~loc
                (A.pmod_ident ~loc (lid ~loc [ functor_name ]))
                (A.pmod_structure ~loc
                   (List.map
                      (fun (name, expr) ->
                        A.pstr_value ~loc Nonrecursive
                          [
                            A.value_binding ~loc ~pat:(pvar ~loc name)
                              ~expr;
                          ])
                      bindings))))
    in
    let schema_policy_module =
      schema_module "Schema_policy" "With_policy"
        [
          ("query_rules", list ~loc schema_query_rules);
          ("mutation_rules", list ~loc schema_mutation_rules);
        ]
    in
    let schema_hooks_module =
      schema_module "Schema_hooks" "With_hooks"
        [ ("mutation_hooks", list ~loc schema_mutation_hooks) ]
    in
    let schema_interceptors_module =
      schema_module "Schema_interceptors" "With_interceptors"
        [ ("query_interceptors", list ~loc schema_query_interceptors) ]
    in
    let structure =
      [
        value_fun "all"
          (A.pexp_fun ~loc Nolabel None (pvar ~loc "ctx")
             (A.pexp_fun ~loc (Labelled "decode") None (pvar ~loc "decode")
                (A.pexp_fun ~loc Nolabel None query_pat
                   (backend_apply "find_as"
                      [
                        (Nolabel, evar ~loc "ctx");
                        (Nolabel, evar ~loc "query");
                        (Labelled "decode", evar ~loc "decode");
                      ]))));
        value_fun "one"
          (A.pexp_fun ~loc Nolabel None (pvar ~loc "ctx")
             (A.pexp_fun ~loc (Labelled "decode") None (pvar ~loc "decode")
                (A.pexp_fun ~loc Nolabel None query_pat
                   (backend_apply "find_one_as"
                      [
                        (Nolabel, evar ~loc "ctx");
                        (Nolabel, evar ~loc "query");
                        (Labelled "decode", evar ~loc "decode");
                      ]))));
        value_fun "values"
          (A.pexp_fun ~loc Nolabel None (pvar ~loc "ctx")
             (A.pexp_fun ~loc Nolabel None query_pat
                (backend_apply "values"
                   [
                     (Nolabel, evar ~loc "ctx");
                     (Nolabel, evar ~loc "query");
                   ])));
        value_fun "value"
          (A.pexp_fun ~loc Nolabel None (pvar ~loc "ctx")
             (A.pexp_fun ~loc Nolabel None query_pat
                (backend_apply "value"
                   [
                     (Nolabel, evar ~loc "ctx");
                     (Nolabel, evar ~loc "query");
                   ])));
        value_fun "traverse"
          (A.pexp_fun ~loc Nolabel None (pvar ~loc "ctx")
             (A.pexp_fun ~loc (Labelled "decode") None (pvar ~loc "decode")
                (A.pexp_fun ~loc Nolabel None (pvar ~loc "edge_query")
                   (backend_apply "traverse_as"
                      [
                        (Nolabel, evar ~loc "ctx");
                        (Nolabel, evar ~loc "edge_query");
                        (Labelled "decode", evar ~loc "decode");
                      ]))));
        value_fun "load_edge"
          (A.pexp_fun ~loc Nolabel None (pvar ~loc "ctx")
             (A.pexp_fun ~loc (Labelled "decode_source") None
                (pvar ~loc "decode_source")
                (A.pexp_fun ~loc (Labelled "decode_target") None
                   (pvar ~loc "decode_target")
                   (A.pexp_fun ~loc Nolabel None (pvar ~loc "edge_query")
                      (backend_apply "load_edge_as"
                         [
                           (Nolabel, evar ~loc "ctx");
                           (Nolabel, evar ~loc "edge_query");
                           (Labelled "decode_source", evar ~loc "decode_source");
                           (Labelled "decode_target", evar ~loc "decode_target");
                         ])))));
        value_fun "count"
          (A.pexp_fun ~loc Nolabel None (pvar ~loc "ctx")
             (A.pexp_fun ~loc Nolabel None query_pat
                (backend_apply "count"
                   [
                     (Nolabel, evar ~loc "ctx");
                     (Nolabel, evar ~loc "query");
                   ])));
        value_fun "aggregate"
          (A.pexp_fun ~loc Nolabel None (pvar ~loc "ctx")
             (A.pexp_fun ~loc Nolabel None (pvar ~loc "aggregate")
                (backend_apply "aggregate"
                   [
                     (Nolabel, evar ~loc "ctx");
                     (Nolabel, evar ~loc "aggregate");
                   ])));
        value_fun "aggregate_scan"
          (A.pexp_fun ~loc Nolabel None (pvar ~loc "ctx")
             (A.pexp_fun ~loc Nolabel None (pvar ~loc "scan")
                (backend_apply "aggregate_scan"
                   [
                     (Nolabel, evar ~loc "ctx");
                     (Nolabel, evar ~loc "scan");
                   ])));
        value_fun "group"
          (A.pexp_fun ~loc Nolabel None (pvar ~loc "ctx")
             (A.pexp_fun ~loc Nolabel None (pvar ~loc "group")
                (backend_apply "group"
                   [
                     (Nolabel, evar ~loc "ctx");
                     (Nolabel, evar ~loc "group");
                   ])));
        value_fun "insert"
          (A.pexp_fun ~loc Nolabel None (pvar ~loc "ctx")
             (A.pexp_fun ~loc Nolabel None (pvar ~loc "mutation")
                (backend_apply "insert_values"
                   [
                     (Nolabel, evar ~loc "ctx");
                     (Nolabel, evar ~loc "mutation");
                   ])));
        value_fun "insert_many"
          (A.pexp_fun ~loc (Optional "ordered") None (pvar ~loc "ordered")
             (A.pexp_fun ~loc Nolabel None (pvar ~loc "ctx")
                (A.pexp_fun ~loc Nolabel None (pvar ~loc "mutations")
                   (backend_apply "insert_many_values"
                      [
                        (Optional "ordered", evar ~loc "ordered");
                        (Nolabel, evar ~loc "ctx");
                        (Nolabel, evar ~loc "mutations");
                      ]))));
        value_fun "update_one"
          (A.pexp_fun ~loc Nolabel None (pvar ~loc "ctx")
             (A.pexp_fun ~loc Nolabel None (pvar ~loc "mutation")
                (backend_apply "update_one"
                   [
                     (Nolabel, evar ~loc "ctx");
                     (Nolabel, evar ~loc "mutation");
                   ])));
        value_fun "update"
          (A.pexp_fun ~loc Nolabel None (pvar ~loc "ctx")
             (A.pexp_fun ~loc Nolabel None (pvar ~loc "mutation")
                (backend_apply "update"
                   [
                     (Nolabel, evar ~loc "ctx");
                     (Nolabel, evar ~loc "mutation");
                   ])));
        value_fun "upsert_one"
          (A.pexp_fun ~loc Nolabel None (pvar ~loc "ctx")
             (A.pexp_fun ~loc Nolabel None (pvar ~loc "mutation")
                (backend_apply "upsert_one"
                   [
                     (Nolabel, evar ~loc "ctx");
                     (Nolabel, evar ~loc "mutation");
                   ])));
        value_fun "delete"
          (A.pexp_fun ~loc Nolabel None (pvar ~loc "ctx")
             (A.pexp_fun ~loc Nolabel None (pvar ~loc "mutation")
                (backend_apply "delete"
                   [
                     (Nolabel, evar ~loc "ctx");
                     (Nolabel, evar ~loc "mutation");
                  ])));
        with_policy_module;
        with_hooks_module;
        with_interceptors_module;
        schema_policy_module;
        schema_hooks_module;
        schema_interceptors_module;
      ]
    in
    A.pstr_module ~loc
      (A.module_binding ~loc ~name:{ loc; txt = Some "Store" }
         ~expr:
           (A.pmod_functor ~loc
              (Named ({ loc; txt = Some "Backend" }, backend_type))
              (A.pmod_structure ~loc structure)))
  in
  let client_module =
    let backend_type =
      A.pmty_ident ~loc (lid ~loc [ "Ent_ocaml"; "STORE_BACKEND" ])
    in
    let store_apply name args =
      A.pexp_apply ~loc
        (A.pexp_ident ~loc (lid ~loc [ "Entity_store"; name ]))
        args
    in
    let value_fun name body =
      A.pstr_value ~loc Nonrecursive
        [ A.value_binding ~loc ~pat:(pvar ~loc name) ~expr:body ]
    in
    let client_call name =
      A.pexp_fun ~loc Nolabel None (pvar ~loc "client")
        (A.pexp_fun ~loc Nolabel None (pvar ~loc "value")
           (store_apply name
              [ (Nolabel, evar ~loc "client"); (Nolabel, evar ~loc "value") ]))
    in
    let client_decode_call name =
      A.pexp_fun ~loc Nolabel None (pvar ~loc "client")
        (A.pexp_fun ~loc (Labelled "decode") None (pvar ~loc "decode")
           (A.pexp_fun ~loc Nolabel None (pvar ~loc "value")
              (store_apply name
                 [
                   (Nolabel, evar ~loc "client");
                   (Labelled "decode", evar ~loc "decode");
                   (Nolabel, evar ~loc "value");
                 ])))
    in
    let client_load_edge_call =
      A.pexp_fun ~loc Nolabel None (pvar ~loc "client")
        (A.pexp_fun ~loc (Labelled "decode_source") None
           (pvar ~loc "decode_source")
           (A.pexp_fun ~loc (Labelled "decode_target") None
              (pvar ~loc "decode_target")
              (A.pexp_fun ~loc Nolabel None (pvar ~loc "edge_query")
                 (store_apply "load_edge"
                    [
                      (Nolabel, evar ~loc "client");
                      (Labelled "decode_source", evar ~loc "decode_source");
                      (Labelled "decode_target", evar ~loc "decode_target");
                      (Nolabel, evar ~loc "edge_query");
                    ]))))
    in
    let client_insert_many_call =
      A.pexp_fun ~loc (Optional "ordered") None (pvar ~loc "ordered")
        (A.pexp_fun ~loc Nolabel None (pvar ~loc "client")
           (A.pexp_fun ~loc Nolabel None (pvar ~loc "mutations")
              (store_apply "insert_many"
                 [
                   (Optional "ordered", evar ~loc "ordered");
                   (Nolabel, evar ~loc "client");
                   (Nolabel, evar ~loc "mutations");
                 ])))
    in
    let client_values =
      [
        value_fun "all" (client_decode_call "all");
        value_fun "one" (client_decode_call "one");
        value_fun "values" (client_call "values");
        value_fun "value" (client_call "value");
        value_fun "traverse" (client_decode_call "traverse");
        value_fun "load_edge" client_load_edge_call;
        value_fun "count" (client_call "count");
        value_fun "aggregate" (client_call "aggregate");
        value_fun "aggregate_scan" (client_call "aggregate_scan");
        value_fun "group" (client_call "group");
        value_fun "insert" (client_call "insert");
        value_fun "insert_many" client_insert_many_call;
        value_fun "update_one" (client_call "update_one");
        value_fun "update" (client_call "update");
        value_fun "upsert_one" (client_call "upsert_one");
        value_fun "delete" (client_call "delete");
      ]
    in
    let tx_module =
      A.pstr_module ~loc
        (A.module_binding ~loc ~name:{ loc; txt = Some "Tx" }
           ~expr:
             (A.pmod_structure ~loc
                ([
                   A.pstr_type ~loc Recursive
                     [
                       A.type_declaration ~loc ~name:{ loc; txt = "t" }
                         ~params:[] ~cstrs:[] ~kind:Ptype_abstract
                         ~private_:Public
                         ~manifest:
                           (Some
                              (A.ptyp_constr ~loc
                                 (lid ~loc [ "Backend"; "ctx" ])
                                 []));
                     ];
                   value_fun "ctx"
                     (A.pexp_fun ~loc Nolabel None (pvar ~loc "client")
                        (evar ~loc "client"));
                 ]
                @ client_values)))
    in
    let structure =
      [
        A.pstr_module ~loc
          (A.module_binding ~loc ~name:{ loc; txt = Some "Entity_store" }
             ~expr:
               (A.pmod_apply ~loc
                  (A.pmod_ident ~loc (lid ~loc [ "Store" ]))
                  (A.pmod_ident ~loc (lid ~loc [ "Backend" ]))));
        A.pstr_type ~loc Recursive
          [
            A.type_declaration ~loc ~name:{ loc; txt = "t" } ~params:[]
              ~cstrs:[] ~kind:Ptype_abstract ~private_:Public
              ~manifest:
                (Some
                   (A.ptyp_constr ~loc (lid ~loc [ "Backend"; "ctx" ]) []));
          ];
        value_fun "make"
          (A.pexp_fun ~loc Nolabel None (pvar ~loc "ctx") (evar ~loc "ctx"));
        value_fun "ctx"
          (A.pexp_fun ~loc Nolabel None (pvar ~loc "client")
             (evar ~loc "client"));
        tx_module;
        value_fun "with_transaction"
          (A.pexp_fun ~loc Nolabel None (pvar ~loc "client")
             (A.pexp_fun ~loc Nolabel None (pvar ~loc "f")
                (A.pexp_apply ~loc
                   (ident ~loc [ "Backend"; "transaction" ])
                   [
                     (Nolabel, evar ~loc "client");
                     ( Nolabel,
                       A.pexp_fun ~loc Nolabel None (pvar ~loc "tx_ctx")
                         (A.pexp_apply ~loc (evar ~loc "f")
                            [ (Nolabel, evar ~loc "tx_ctx") ]) );
                   ])));
      ]
      @ client_values
    in
    A.pstr_module ~loc
      (A.module_binding ~loc ~name:{ loc; txt = Some "Client" }
         ~expr:
           (A.pmod_functor ~loc
              (Named ({ loc; txt = Some "Backend" }, backend_type))
              (A.pmod_structure ~loc structure)))
  in
  let structure =
    query :: boolean_predicates :: query_pipe_helpers :: aggregate_helpers
    :: create :: create_values
    :: create_result :: create_values_result :: create_record
    :: create_record_result :: create_many :: create_many_result
    :: create_many_values :: create_many_values_result :: mutation_pipe_helpers
    :: update_fn "update_one" "Update_one"
    :: update_fn "update" "Update"
    :: update_result_fn "update_one_result" "Update_one"
    :: update_result_fn "update_result" "Update"
    :: update_query_fn "update_one_where" "Update_one"
    :: update_query_fn "update_where" "Update"
    :: update_query_result_fn "update_one_where_result" "Update_one"
    :: update_query_result_fn "update_where_result" "Update"
    :: upsert_fn
    :: upsert_query_fn
    :: delete_fn "delete_one" "Delete_one"
    :: delete_fn "delete" "Delete"
    :: delete_query_fn "delete_one_where" "Delete_one"
    :: delete_query_fn "delete_where" "Delete"
    :: store_module
    :: client_module
    :: (List.concat_map edge_helper_items edges
       @ List.concat_map field_helper_items fields
       @ id_helper_items)
  in
  A.pstr_module ~loc
    (A.module_binding ~loc ~name:{ loc; txt = Some module_name }
       ~expr:(A.pmod_structure ~loc structure))

let generate_str ~loc:_ ~path:_ (_rec_flag, tds) =
  List.concat_map
    (fun td ->
      [ gen_entity td; gen_schema_snapshot td; gen_value_converter td; gen_query_module td ])
    tds

let gen_sig_for_type td =
  let loc = loc_of_type_decl td in
  let fields = ensure_record td in
  let edges = edge_names td in
  let type_name = td.ptype_name.txt in
  let module_name = snake_to_pascal type_name in
  let typ = A.ptyp_constr ~loc (lid ~loc [ "Ent_ocaml"; "entity" ]) [] in
  let record_typ = A.ptyp_constr ~loc (lid ~loc [ type_name ]) [] in
  let value_converter_typ =
    A.ptyp_arrow ~loc Nolabel record_typ
      (A.ptyp_constr ~loc (lid ~loc [ "Ent_ocaml"; "value" ]) [])
  in
  let predicate_typ =
    A.ptyp_constr ~loc (lid ~loc [ "Ent_ocaml"; "predicate" ]) []
  in
  let order_typ = A.ptyp_constr ~loc (lid ~loc [ "Ent_ocaml"; "order" ]) [] in
  let cursor_term_typ =
    A.ptyp_constr ~loc (lid ~loc [ "Ent_ocaml"; "cursor_term" ]) []
  in
  let query_typ = A.ptyp_constr ~loc (lid ~loc [ "Ent_ocaml"; "query" ]) [] in
  let edge_query_typ =
    A.ptyp_constr ~loc (lid ~loc [ "Ent_ocaml"; "edge_query" ]) []
  in
  let entity_typ =
    A.ptyp_constr ~loc (lid ~loc [ "Ent_ocaml"; "entity" ]) []
  in
  let aggregate_typ =
    A.ptyp_constr ~loc (lid ~loc [ "Ent_ocaml"; "aggregate" ]) []
  in
  let aggregate_op_typ =
    A.ptyp_constr ~loc (lid ~loc [ "Ent_ocaml"; "aggregate_op" ]) []
  in
  let aggregate_scan_typ =
    A.ptyp_constr ~loc (lid ~loc [ "Ent_ocaml"; "aggregate_scan" ]) []
  in
  let group_aggregate_typ =
    A.ptyp_constr ~loc (lid ~loc [ "Ent_ocaml"; "group_aggregate" ]) []
  in
  let group_result_typ =
    A.ptyp_constr ~loc (lid ~loc [ "Ent_ocaml"; "group_result" ]) []
  in
  let dynamic_filter_typ =
    A.ptyp_constr ~loc (lid ~loc [ "Ent_ocaml"; "Dynamic_filter"; "t" ]) []
  in
  let dynamic_filter_op_typ =
    A.ptyp_constr ~loc (lid ~loc [ "Ent_ocaml"; "Dynamic_filter"; "op" ]) []
  in
  let mutation_typ =
    A.ptyp_constr ~loc (lid ~loc [ "Ent_ocaml"; "mutation" ]) []
  in
  let value_typ = A.ptyp_constr ~loc (lid ~loc [ "Ent_ocaml"; "value" ]) [] in
  let direction_typ =
    A.ptyp_constr ~loc (lid ~loc [ "Ent_ocaml"; "order_direction" ]) []
  in
  let string_typ = A.ptyp_constr ~loc (lid ~loc [ "string" ]) [] in
  let bool_typ = A.ptyp_constr ~loc (lid ~loc [ "bool" ]) [] in
  let int_typ = A.ptyp_constr ~loc (lid ~loc [ "int" ]) [] in
  let unit_typ = A.ptyp_constr ~loc (lid ~loc [ "unit" ]) [] in
  let list_typ typ = A.ptyp_constr ~loc (lid ~loc [ "list" ]) [ typ ] in
  let pair_typ left right =
    A.ptyp_tuple ~loc [ left; right ]
  in
  let field_value_typ = pair_typ string_typ value_typ in
  let field_values_typ = list_typ field_value_typ in
  let predicates_typ = list_typ predicate_typ in
  let orders_typ = list_typ order_typ in
  let error_typ = A.ptyp_constr ~loc (lid ~loc [ "Ent_ocaml"; "error" ]) [] in
  let result_typ ok err = A.ptyp_constr ~loc (lid ~loc [ "result" ]) [ ok; err ] in
  let value_decoder_typ =
    A.ptyp_arrow ~loc Nolabel value_typ
      (result_typ record_typ string_typ)
  in
  let arrow label arg result = A.ptyp_arrow ~loc label arg result in
  let val_sig name type_ =
    A.psig_value ~loc (A.value_description ~loc ~name:{ loc; txt = name } ~type_ ~prim:[])
  in
  let query_sig =
    val_sig "query"
      (arrow (Optional "where") predicates_typ
         (arrow (Optional "select") (list_typ string_typ)
            (arrow (Optional "order") orders_typ
               (arrow (Optional "limit") int_typ
                  (arrow (Optional "offset") int_typ
                     (arrow Nolabel unit_typ query_typ))))))
  in
  let create_sig =
    val_sig "create" (arrow Nolabel unit_typ mutation_typ)
  in
  let create_values_sig =
    val_sig "create_values" (arrow Nolabel field_values_typ mutation_typ)
  in
  let create_result_sig =
    val_sig "create_result"
      (arrow Nolabel unit_typ (result_typ mutation_typ error_typ))
  in
  let create_values_result_sig =
    val_sig "create_values_result"
      (arrow Nolabel field_values_typ (result_typ mutation_typ error_typ))
  in
  let create_record_sig =
    val_sig "create_record" (arrow Nolabel record_typ mutation_typ)
  in
  let create_record_result_sig =
    val_sig "create_record_result"
      (arrow Nolabel record_typ (result_typ mutation_typ error_typ))
  in
  let create_many_sig =
    val_sig "create_many"
      (arrow Nolabel (list_typ record_typ) (list_typ mutation_typ))
  in
  let create_many_result_sig =
    val_sig "create_many_result"
      (arrow Nolabel (list_typ record_typ)
         (result_typ (list_typ mutation_typ) error_typ))
  in
  let create_many_values_sig =
    val_sig "create_many_values"
      (arrow Nolabel (list_typ field_values_typ) (list_typ mutation_typ))
  in
  let create_many_values_result_sig =
    val_sig "create_many_values_result"
      (arrow Nolabel (list_typ field_values_typ)
         (result_typ (list_typ mutation_typ) error_typ))
  in
  let mutation_pipe_sig =
    [
      val_sig "set" (arrow Nolabel field_value_typ (arrow Nolabel mutation_typ mutation_typ));
      val_sig "set_all"
        (arrow Nolabel field_values_typ (arrow Nolabel mutation_typ mutation_typ));
      val_sig "clear" (arrow Nolabel string_typ (arrow Nolabel mutation_typ mutation_typ));
      val_sig "add" (arrow Nolabel field_value_typ (arrow Nolabel mutation_typ mutation_typ));
      val_sig "on_insert"
        (arrow Nolabel field_value_typ (arrow Nolabel mutation_typ mutation_typ));
      val_sig "on_insert_all"
        (arrow Nolabel field_values_typ (arrow Nolabel mutation_typ mutation_typ));
    ]
  in
  let boolean_sig =
    [
      val_sig "and_" (arrow Nolabel predicates_typ predicate_typ);
      val_sig "or_" (arrow Nolabel predicates_typ predicate_typ);
      val_sig "not_" (arrow Nolabel predicate_typ predicate_typ);
      val_sig "dynamic_filter"
        (arrow (Optional "value") value_typ
           (arrow (Labelled "field") string_typ
              (arrow Nolabel dynamic_filter_op_typ dynamic_filter_typ)));
      val_sig "dynamic_predicate"
        (arrow Nolabel dynamic_filter_typ
           (result_typ predicate_typ error_typ));
      val_sig "entql_predicate"
        (arrow Nolabel string_typ (result_typ predicate_typ error_typ));
    ]
  in
  let query_pipe_sig =
    [
      val_sig "where" (arrow Nolabel predicate_typ (arrow Nolabel query_typ query_typ));
      val_sig "where_all"
        (arrow Nolabel predicates_typ (arrow Nolabel query_typ query_typ));
      val_sig "where_dynamic"
        (arrow Nolabel dynamic_filter_typ
           (arrow Nolabel query_typ (result_typ query_typ error_typ)));
      val_sig "where_dynamic_all"
        (arrow Nolabel (list_typ dynamic_filter_typ)
           (arrow Nolabel query_typ (result_typ query_typ error_typ)));
      val_sig "where_entql"
        (arrow Nolabel string_typ
           (arrow Nolabel query_typ (result_typ query_typ error_typ)));
      val_sig "select"
        (arrow Nolabel (list_typ string_typ) (arrow Nolabel query_typ query_typ));
      val_sig "order_by"
        (arrow Nolabel orders_typ (arrow Nolabel query_typ query_typ));
      val_sig "limit" (arrow Nolabel int_typ (arrow Nolabel query_typ query_typ));
      val_sig "offset" (arrow Nolabel int_typ (arrow Nolabel query_typ query_typ));
      val_sig "after_cursor"
        (arrow Nolabel (list_typ cursor_term_typ)
           (arrow Nolabel query_typ query_typ));
      val_sig "before_cursor"
        (arrow Nolabel (list_typ cursor_term_typ)
           (arrow Nolabel query_typ query_typ));
    ]
  in
  let aggregate_sig =
    let scan_op_typ = pair_typ string_typ aggregate_op_typ in
    [
      val_sig "count" (arrow Nolabel query_typ aggregate_typ);
      val_sig "min" (arrow Nolabel string_typ (arrow Nolabel query_typ aggregate_typ));
      val_sig "max" (arrow Nolabel string_typ (arrow Nolabel query_typ aggregate_typ));
      val_sig "sum" (arrow Nolabel string_typ (arrow Nolabel query_typ aggregate_typ));
      val_sig "avg" (arrow Nolabel string_typ (arrow Nolabel query_typ aggregate_typ));
      val_sig "count_as" (arrow Nolabel string_typ scan_op_typ);
      val_sig "min_as"
        (arrow Nolabel string_typ (arrow Nolabel string_typ scan_op_typ));
      val_sig "max_as"
        (arrow Nolabel string_typ (arrow Nolabel string_typ scan_op_typ));
      val_sig "sum_as"
        (arrow Nolabel string_typ (arrow Nolabel string_typ scan_op_typ));
      val_sig "avg_as"
        (arrow Nolabel string_typ (arrow Nolabel string_typ scan_op_typ));
      val_sig "scan"
        (arrow Nolabel (list_typ scan_op_typ)
           (arrow Nolabel query_typ aggregate_scan_typ));
      val_sig "group_by"
        (arrow Nolabel string_typ
           (arrow Nolabel aggregate_typ group_aggregate_typ));
    ]
  in
  let edge_sig_items edge_name =
    [
      val_sig ("has_" ^ edge_name) (arrow Nolabel unit_typ predicate_typ);
      val_sig ("has_" ^ edge_name ^ "_with")
        (arrow Nolabel predicates_typ predicate_typ);
      val_sig edge_name (arrow Nolabel predicate_typ predicate_typ);
      val_sig ("query_" ^ edge_name)
        (arrow (Optional "target_query") query_typ
           (arrow (Labelled "target") entity_typ
              (arrow Nolabel query_typ edge_query_typ)));
      val_sig ("with_" ^ edge_name)
        (arrow (Optional "target_query") query_typ
           (arrow (Labelled "target") entity_typ
              (arrow Nolabel query_typ edge_query_typ)));
    ]
  in
  let update_sig name =
    val_sig name
      (arrow (Optional "where") predicates_typ
         (arrow (Optional "set") field_values_typ
            (arrow (Optional "clear") (list_typ string_typ)
               (arrow (Optional "add") field_values_typ
                  (arrow Nolabel unit_typ mutation_typ)))))
  in
  let update_result_sig name =
    val_sig name
      (arrow (Optional "where") predicates_typ
         (arrow (Optional "set") field_values_typ
            (arrow (Optional "clear") (list_typ string_typ)
               (arrow (Optional "add") field_values_typ
                  (arrow Nolabel unit_typ
                     (result_typ mutation_typ error_typ))))))
  in
  let update_query_sig name =
    val_sig name
      (arrow (Optional "set") field_values_typ
         (arrow (Optional "clear") (list_typ string_typ)
            (arrow (Optional "add") field_values_typ
               (arrow Nolabel query_typ mutation_typ))))
  in
  let update_query_result_sig name =
    val_sig name
      (arrow (Optional "set") field_values_typ
         (arrow (Optional "clear") (list_typ string_typ)
            (arrow (Optional "add") field_values_typ
               (arrow Nolabel query_typ
                  (result_typ mutation_typ error_typ)))))
  in
  let upsert_sig name =
    val_sig name
      (arrow (Optional "where") predicates_typ
         (arrow Nolabel unit_typ mutation_typ))
  in
  let upsert_query_sig name =
    val_sig name (arrow Nolabel query_typ mutation_typ)
  in
  let delete_sig name =
    val_sig name
      (arrow (Optional "where") predicates_typ
         (arrow Nolabel unit_typ mutation_typ))
  in
  let delete_query_sig name =
    val_sig name (arrow Nolabel query_typ mutation_typ)
  in
  let id_sig_items =
    match
      List.find_opt
        (fun field ->
          field.pld_name.txt = "id" && Option.is_some (value_constructor field))
        fields
    with
    | None -> []
    | Some field ->
        [
          val_sig "by_id" (arrow Nolabel field.pld_type query_typ);
          val_sig "update_id" (arrow Nolabel field.pld_type mutation_typ);
          val_sig "update_id_result"
            (arrow Nolabel field.pld_type (result_typ mutation_typ error_typ));
          val_sig "delete_id" (arrow Nolabel field.pld_type mutation_typ);
        ]
  in
  let field_sig_items field =
    let field_name = field.pld_name.txt in
    let field_typ = field.pld_type in
    let order_sig =
      val_sig (field_name ^ "_order")
        (arrow (Optional "direction") direction_typ
           (arrow (Optional "as_") string_typ
              (arrow Nolabel unit_typ order_typ)))
    in
    let selector_sig = val_sig ("select_" ^ field_name) string_typ in
    let value_sig =
      match value_constructor field with
      | None -> []
      | Some _ -> [ val_sig field_name (arrow Nolabel field_typ field_value_typ) ]
    in
    let json_helpers =
      if is_json_field field then
        [
          val_sig (field_name ^ "_path_eq")
            (arrow Nolabel (list_typ string_typ)
               (arrow Nolabel value_typ predicate_typ));
          val_sig (field_name ^ "_path_neq")
            (arrow Nolabel (list_typ string_typ)
               (arrow Nolabel value_typ predicate_typ));
          val_sig (field_name ^ "_path_gt")
            (arrow Nolabel (list_typ string_typ)
               (arrow Nolabel value_typ predicate_typ));
          val_sig (field_name ^ "_path_gte")
            (arrow Nolabel (list_typ string_typ)
               (arrow Nolabel value_typ predicate_typ));
          val_sig (field_name ^ "_path_lt")
            (arrow Nolabel (list_typ string_typ)
               (arrow Nolabel value_typ predicate_typ));
          val_sig (field_name ^ "_path_lte")
            (arrow Nolabel (list_typ string_typ)
               (arrow Nolabel value_typ predicate_typ));
          val_sig (field_name ^ "_path_in")
            (arrow Nolabel (list_typ string_typ)
               (arrow Nolabel (list_typ value_typ) predicate_typ));
          val_sig (field_name ^ "_path_not_in")
            (arrow Nolabel (list_typ string_typ)
               (arrow Nolabel (list_typ value_typ) predicate_typ));
          val_sig (field_name ^ "_path_is_nil")
            (arrow Nolabel (list_typ string_typ) predicate_typ);
          val_sig (field_name ^ "_path_not_nil")
            (arrow Nolabel (list_typ string_typ) predicate_typ);
          val_sig (field_name ^ "_path_order")
            (arrow (Optional "direction") direction_typ
               (arrow (Optional "as_") string_typ
                  (arrow Nolabel (list_typ string_typ) order_typ)));
        ]
      else []
    in
    match value_constructor field with
    | None -> value_sig @ [ selector_sig; order_sig ] @ json_helpers
    | Some value_path ->
        let base =
          [
            val_sig (field_name ^ "_eq") (arrow Nolabel field_typ predicate_typ);
            val_sig (field_name ^ "_neq") (arrow Nolabel field_typ predicate_typ);
            val_sig (field_name ^ "_in")
              (arrow Nolabel (list_typ field_typ) predicate_typ);
            val_sig (field_name ^ "_not_in")
              (arrow Nolabel (list_typ field_typ) predicate_typ);
            selector_sig;
            order_sig;
          ]
        in
        let comparison_helpers =
          if is_comparable_field field then
            [
              val_sig (field_name ^ "_gt")
                (arrow Nolabel field_typ predicate_typ);
              val_sig (field_name ^ "_gte")
                (arrow Nolabel field_typ predicate_typ);
              val_sig (field_name ^ "_lt")
                (arrow Nolabel field_typ predicate_typ);
              val_sig (field_name ^ "_lte")
                (arrow Nolabel field_typ predicate_typ);
              val_sig ("after_" ^ field_name)
                (arrow (Optional "direction") direction_typ
                   (arrow Nolabel field_typ
                      (arrow Nolabel query_typ query_typ)));
              val_sig ("before_" ^ field_name)
                (arrow (Optional "direction") direction_typ
                   (arrow Nolabel field_typ
                      (arrow Nolabel query_typ query_typ)));
              val_sig (field_name ^ "_cursor")
                (arrow (Optional "direction") direction_typ
                   (arrow Nolabel field_typ cursor_term_typ));
            ]
          else []
        in
        let nil_helpers =
          if is_option field then
            [
              val_sig (field_name ^ "_is_nil")
                (arrow Nolabel unit_typ predicate_typ);
              val_sig (field_name ^ "_not_nil")
                (arrow Nolabel unit_typ predicate_typ);
            ]
          else []
        in
        let string_helpers =
          match value_path with
          | [ "Ent_ocaml"; "V_string" ] ->
              [
                val_sig (field_name ^ "_contains")
                  (arrow Nolabel string_typ predicate_typ);
                val_sig (field_name ^ "_has_prefix")
                  (arrow Nolabel string_typ predicate_typ);
                val_sig (field_name ^ "_has_suffix")
                  (arrow Nolabel string_typ predicate_typ);
              ]
          | _ -> []
        in
        value_sig @ base @ comparison_helpers @ nil_helpers @ string_helpers
        @ json_helpers
  in
  let store_sig =
    let backend_ctx = A.ptyp_constr ~loc (lid ~loc [ "Backend"; "ctx" ]) [] in
    let backend_doc = A.ptyp_constr ~loc (lid ~loc [ "Backend"; "doc" ]) [] in
    let decode_typ =
      arrow Nolabel backend_doc
        (result_typ (A.ptyp_var ~loc "a") string_typ)
    in
    let decode_source_typ =
      arrow Nolabel backend_doc
        (result_typ (A.ptyp_var ~loc "source") string_typ)
    in
    let decode_target_typ =
      arrow Nolabel backend_doc
        (result_typ (A.ptyp_var ~loc "target") string_typ)
    in
    let value_sig name type_ =
      A.psig_value ~loc
        (A.value_description ~loc ~name:{ loc; txt = name } ~type_ ~prim:[])
    in
    let doc_result = result_typ backend_doc error_typ in
    let docs_result = result_typ (list_typ backend_doc) error_typ in
    let int_result = result_typ int_typ error_typ in
    let unit_result = result_typ unit_typ error_typ in
    let value_option_result =
      result_typ
        (A.ptyp_constr ~loc (lid ~loc [ "option" ]) [ value_typ ])
        error_typ
    in
    let group_result =
      result_typ (list_typ group_result_typ) error_typ
    in
    let aggregate_scan_result =
      result_typ
        (list_typ
           (pair_typ string_typ
              (A.ptyp_constr ~loc (lid ~loc [ "option" ]) [ value_typ ])))
        error_typ
    in
    let base_store_items =
      [
        value_sig "all"
          (arrow Nolabel backend_ctx
             (arrow (Labelled "decode") decode_typ
                (arrow Nolabel query_typ
                   (result_typ (list_typ (A.ptyp_var ~loc "a")) error_typ))));
        value_sig "one"
          (arrow Nolabel backend_ctx
             (arrow (Labelled "decode") decode_typ
                (arrow Nolabel query_typ
                   (result_typ
                      (A.ptyp_constr ~loc (lid ~loc [ "option" ])
                         [ A.ptyp_var ~loc "a" ])
                      error_typ))));
        value_sig "values"
          (arrow Nolabel backend_ctx
             (arrow Nolabel query_typ
                (result_typ (list_typ value_typ) error_typ)));
        value_sig "value"
          (arrow Nolabel backend_ctx
             (arrow Nolabel query_typ
                (result_typ
                   (A.ptyp_constr ~loc (lid ~loc [ "option" ]) [ value_typ ])
                   error_typ)));
        value_sig "traverse"
          (arrow Nolabel backend_ctx
             (arrow (Labelled "decode") decode_typ
                (arrow Nolabel edge_query_typ
                   (result_typ
                      (A.ptyp_constr ~loc (lid ~loc [ "list" ])
                         [ A.ptyp_var ~loc "a" ])
                      error_typ))));
        value_sig "load_edge"
          (arrow Nolabel backend_ctx
             (arrow (Labelled "decode_source") decode_source_typ
                (arrow (Labelled "decode_target") decode_target_typ
                   (arrow Nolabel edge_query_typ
                      (result_typ
                         (list_typ
                            (pair_typ (A.ptyp_var ~loc "source")
                               (A.ptyp_constr ~loc (lid ~loc [ "option" ])
                                  [ A.ptyp_var ~loc "target" ])))
                         error_typ)))));
        value_sig "count" (arrow Nolabel backend_ctx (arrow Nolabel query_typ int_result));
        value_sig "aggregate"
          (arrow Nolabel backend_ctx
             (arrow Nolabel aggregate_typ value_option_result));
        value_sig "aggregate_scan"
          (arrow Nolabel backend_ctx
             (arrow Nolabel aggregate_scan_typ aggregate_scan_result));
        value_sig "group"
          (arrow Nolabel backend_ctx
             (arrow Nolabel group_aggregate_typ group_result));
        value_sig "insert"
          (arrow Nolabel backend_ctx (arrow Nolabel mutation_typ doc_result));
        value_sig "insert_many"
          (arrow (Optional "ordered") bool_typ
             (arrow Nolabel backend_ctx
                (arrow Nolabel (list_typ mutation_typ) docs_result)));
        value_sig "update_one"
          (arrow Nolabel backend_ctx (arrow Nolabel mutation_typ unit_result));
        value_sig "update"
          (arrow Nolabel backend_ctx (arrow Nolabel mutation_typ int_result));
        value_sig "upsert_one"
          (arrow Nolabel backend_ctx (arrow Nolabel mutation_typ unit_result));
        value_sig "delete"
          (arrow Nolabel backend_ctx (arrow Nolabel mutation_typ int_result));
      ]
    in
    let policy_type =
      let query_rule_typ =
        A.ptyp_constr ~loc
          (lid ~loc [ "Ent_ocaml"; "query_rule" ])
          [ backend_ctx ]
      in
      let mutation_rule_typ =
        A.ptyp_constr ~loc
          (lid ~loc [ "Ent_ocaml"; "mutation_rule" ])
          [ backend_ctx ]
      in
      A.pmty_signature ~loc
        [
          value_sig "query_rules" (list_typ query_rule_typ);
          value_sig "mutation_rules" (list_typ mutation_rule_typ);
        ]
    in
    let hooks_type =
      let mutation_hook_typ =
        A.ptyp_constr ~loc
          (lid ~loc [ "Ent_ocaml"; "mutation_hook" ])
          [ backend_ctx ]
      in
      A.pmty_signature ~loc
        [ value_sig "mutation_hooks" (list_typ mutation_hook_typ) ]
    in
    let interceptors_type =
      let query_interceptor_typ =
        A.ptyp_constr ~loc
          (lid ~loc [ "Ent_ocaml"; "query_interceptor" ])
          [ backend_ctx ]
      in
      A.pmty_signature ~loc
        [ value_sig "query_interceptors" (list_typ query_interceptor_typ) ]
    in
    let store_items =
      base_store_items
      @ [
          A.psig_module ~loc
            (A.module_declaration ~loc ~name:{ loc; txt = Some "With_policy" }
               ~type_:
                 (A.pmty_functor ~loc
                    (Named ({ loc; txt = Some "Policy" }, policy_type))
                    (A.pmty_signature ~loc base_store_items)));
          A.psig_module ~loc
            (A.module_declaration ~loc ~name:{ loc; txt = Some "With_hooks" }
               ~type_:
                 (A.pmty_functor ~loc
                    (Named ({ loc; txt = Some "Hooks" }, hooks_type))
                    (A.pmty_signature ~loc base_store_items)));
          A.psig_module ~loc
            (A.module_declaration ~loc
               ~name:{ loc; txt = Some "With_interceptors" }
               ~type_:
                 (A.pmty_functor ~loc
                    (Named
                       ( { loc; txt = Some "Interceptors" },
                         interceptors_type ))
                    (A.pmty_signature ~loc base_store_items)));
          A.psig_module ~loc
            (A.module_declaration ~loc ~name:{ loc; txt = Some "Schema_policy" }
               ~type_:(A.pmty_signature ~loc base_store_items));
          A.psig_module ~loc
            (A.module_declaration ~loc ~name:{ loc; txt = Some "Schema_hooks" }
               ~type_:(A.pmty_signature ~loc base_store_items));
          A.psig_module ~loc
            (A.module_declaration ~loc
               ~name:{ loc; txt = Some "Schema_interceptors" }
               ~type_:(A.pmty_signature ~loc base_store_items));
        ]
    in
    A.psig_module ~loc
      (A.module_declaration ~loc ~name:{ loc; txt = Some "Store" }
         ~type_:
           (A.pmty_functor ~loc
              (Named
                 ( { loc; txt = Some "Backend" },
                   A.pmty_ident ~loc
                     (lid ~loc [ "Ent_ocaml"; "STORE_BACKEND" ]) ))
              (A.pmty_signature ~loc store_items)))
  in
  let client_sig =
    let backend_ctx = A.ptyp_constr ~loc (lid ~loc [ "Backend"; "ctx" ]) [] in
    let backend_doc = A.ptyp_constr ~loc (lid ~loc [ "Backend"; "doc" ]) [] in
    let client_t = A.ptyp_constr ~loc (lid ~loc [ "t" ]) [] in
    let tx_t = A.ptyp_constr ~loc (lid ~loc [ "Tx"; "t" ]) [] in
    let decode_typ =
      arrow Nolabel backend_doc
        (result_typ (A.ptyp_var ~loc "a") string_typ)
    in
    let decode_source_typ =
      arrow Nolabel backend_doc
        (result_typ (A.ptyp_var ~loc "source") string_typ)
    in
    let decode_target_typ =
      arrow Nolabel backend_doc
        (result_typ (A.ptyp_var ~loc "target") string_typ)
    in
    let value_sig name type_ =
      A.psig_value ~loc
        (A.value_description ~loc ~name:{ loc; txt = name } ~type_ ~prim:[])
    in
    let doc_result = result_typ backend_doc error_typ in
    let docs_result = result_typ (list_typ backend_doc) error_typ in
    let int_result = result_typ int_typ error_typ in
    let unit_result = result_typ unit_typ error_typ in
    let value_option_result =
      result_typ
        (A.ptyp_constr ~loc (lid ~loc [ "option" ]) [ value_typ ])
        error_typ
    in
    let group_result = result_typ (list_typ group_result_typ) error_typ in
    let aggregate_scan_result =
      result_typ
        (list_typ
           (pair_typ string_typ
              (A.ptyp_constr ~loc (lid ~loc [ "option" ]) [ value_typ ])))
        error_typ
    in
    let operation_items receiver_t =
      [
        value_sig "all"
          (arrow Nolabel receiver_t
             (arrow (Labelled "decode") decode_typ
                (arrow Nolabel query_typ
                   (result_typ (list_typ (A.ptyp_var ~loc "a")) error_typ))));
        value_sig "one"
          (arrow Nolabel receiver_t
             (arrow (Labelled "decode") decode_typ
                (arrow Nolabel query_typ
                   (result_typ
                      (A.ptyp_constr ~loc (lid ~loc [ "option" ])
                         [ A.ptyp_var ~loc "a" ])
                      error_typ))));
        value_sig "values"
          (arrow Nolabel receiver_t
             (arrow Nolabel query_typ
                (result_typ (list_typ value_typ) error_typ)));
        value_sig "value"
          (arrow Nolabel receiver_t
             (arrow Nolabel query_typ
                (result_typ
                   (A.ptyp_constr ~loc (lid ~loc [ "option" ]) [ value_typ ])
                   error_typ)));
        value_sig "traverse"
          (arrow Nolabel receiver_t
             (arrow (Labelled "decode") decode_typ
                (arrow Nolabel edge_query_typ
                   (result_typ (list_typ (A.ptyp_var ~loc "a")) error_typ))));
        value_sig "load_edge"
          (arrow Nolabel receiver_t
             (arrow (Labelled "decode_source") decode_source_typ
                (arrow (Labelled "decode_target") decode_target_typ
                   (arrow Nolabel edge_query_typ
                      (result_typ
                         (list_typ
                            (pair_typ (A.ptyp_var ~loc "source")
                               (A.ptyp_constr ~loc (lid ~loc [ "option" ])
                                  [ A.ptyp_var ~loc "target" ])))
                         error_typ)))));
        value_sig "count" (arrow Nolabel receiver_t (arrow Nolabel query_typ int_result));
        value_sig "aggregate"
          (arrow Nolabel receiver_t
             (arrow Nolabel aggregate_typ value_option_result));
        value_sig "aggregate_scan"
          (arrow Nolabel receiver_t
             (arrow Nolabel aggregate_scan_typ aggregate_scan_result));
        value_sig "group"
          (arrow Nolabel receiver_t
             (arrow Nolabel group_aggregate_typ group_result));
        value_sig "insert"
          (arrow Nolabel receiver_t (arrow Nolabel mutation_typ doc_result));
        value_sig "insert_many"
          (arrow (Optional "ordered") bool_typ
             (arrow Nolabel receiver_t
                (arrow Nolabel (list_typ mutation_typ) docs_result)));
        value_sig "update_one"
          (arrow Nolabel receiver_t (arrow Nolabel mutation_typ unit_result));
        value_sig "update"
          (arrow Nolabel receiver_t (arrow Nolabel mutation_typ int_result));
        value_sig "upsert_one"
          (arrow Nolabel receiver_t (arrow Nolabel mutation_typ unit_result));
        value_sig "delete"
          (arrow Nolabel receiver_t (arrow Nolabel mutation_typ int_result));
      ]
    in
    let tx_items =
      [
        A.psig_type ~loc Recursive
          [
            A.type_declaration ~loc ~name:{ loc; txt = "t" } ~params:[]
              ~cstrs:[] ~kind:Ptype_abstract ~private_:Public ~manifest:None;
          ];
        value_sig "ctx" (arrow Nolabel client_t backend_ctx);
      ]
      @ operation_items client_t
    in
    let client_items =
      [
        A.psig_type ~loc Recursive
          [
            A.type_declaration ~loc ~name:{ loc; txt = "t" } ~params:[]
              ~cstrs:[] ~kind:Ptype_abstract ~private_:Public ~manifest:None;
          ];
        value_sig "make" (arrow Nolabel backend_ctx client_t);
        value_sig "ctx" (arrow Nolabel client_t backend_ctx);
        A.psig_module ~loc
          (A.module_declaration ~loc ~name:{ loc; txt = Some "Tx" }
             ~type_:(A.pmty_signature ~loc tx_items));
        value_sig "with_transaction"
          (arrow Nolabel client_t
             (arrow Nolabel
                (arrow Nolabel tx_t
                   (result_typ (A.ptyp_var ~loc "a") error_typ))
                (result_typ (A.ptyp_var ~loc "a") error_typ)));
      ]
      @ operation_items client_t
    in
    A.psig_module ~loc
      (A.module_declaration ~loc ~name:{ loc; txt = Some "Client" }
         ~type_:
           (A.pmty_functor ~loc
              (Named
                 ( { loc; txt = Some "Backend" },
                   A.pmty_ident ~loc
                     (lid ~loc [ "Ent_ocaml"; "STORE_BACKEND" ]) ))
              (A.pmty_signature ~loc client_items)))
  in
  let module_items =
    [ query_sig ]
    @ boolean_sig @ query_pipe_sig @ aggregate_sig
    @ [
        create_sig;
        create_values_sig;
        create_result_sig;
        create_values_result_sig;
        create_record_sig;
        create_record_result_sig;
        create_many_sig;
        create_many_result_sig;
        create_many_values_sig;
        create_many_values_result_sig;
      ]
    @ mutation_pipe_sig
    @ [
        update_sig "update_one";
        update_sig "update";
        update_result_sig "update_one_result";
        update_result_sig "update_result";
        update_query_sig "update_one_where";
        update_query_sig "update_where";
        update_query_result_sig "update_one_where_result";
        update_query_result_sig "update_where_result";
        upsert_sig "upsert_one";
        upsert_query_sig "upsert_where";
        delete_sig "delete_one";
        delete_sig "delete";
        delete_query_sig "delete_one_where";
        delete_query_sig "delete_where";
        store_sig;
        client_sig;
      ]
    @ id_sig_items
    @ List.concat_map edge_sig_items edges
    @ List.concat_map field_sig_items fields
  in
  [
    A.psig_value ~loc
      (A.value_description ~loc
         ~name:{ loc; txt = type_name ^ "_entity" }
         ~type_:typ ~prim:[]);
    A.psig_value ~loc
      (A.value_description ~loc
         ~name:{ loc; txt = type_name ^ "_schema_snapshot" }
         ~type_:value_typ ~prim:[]);
    A.psig_value ~loc
      (A.value_description ~loc
         ~name:{ loc; txt = type_name ^ "_to_ent_value" }
         ~type_:value_converter_typ ~prim:[]);
    A.psig_value ~loc
      (A.value_description ~loc
         ~name:{ loc; txt = type_name ^ "_of_ent_value" }
         ~type_:value_decoder_typ ~prim:[]);
    A.psig_module ~loc
      (A.module_declaration ~loc ~name:{ loc; txt = Some module_name }
         ~type_:(A.pmty_signature ~loc module_items));
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
