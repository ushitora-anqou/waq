open Ppxlib

type typ =
  [ `Int
  | `String
  | `Ptime
  | `ID of longident (* M.ID.t *)
  | `User of longident
  | `Option of typ ]

let rec core_type_of_typ loc : typ -> core_type =
  let open Ast_builder.Default in
  function
  | `Int -> [%type: int]
  | `String -> [%type: string]
  | `Ptime -> [%type: Ptime.t]
  | `ID l | `User l -> ptyp_constr ~loc { loc; txt = l } []
  | `Option t ->
      ptyp_constr ~loc { loc; txt = lident "option" } [ core_type_of_typ loc t ]

type column = { c_ocaml_name : string; c_sql_name : string; c_typ : typ }

type schema = {
  s_sql_name : string;
  s_columns : column list;
  s_code_path : Code_path.t;
}

let state = Hashtbl.create 0

module Schema = struct
  let parse_config (x : structure_item) =
    (* Parse config e.g., `name "notifications"` *)
    let open Ast_pattern in
    parse
      (pstr_eval (pexp_apply (pexp_ident (lident __)) (__ ^:: nil)) drop)
      x.pstr_loc x
    @@ fun (key : string) (_, (v : expression)) ->
    match (key, v.pexp_desc) with
    | "name", Pexp_constant (Pconst_string (name, _, _)) -> `Name name
    | _ -> assert false

  let parse_column (x : structure_item) =
    (* Parse column e.g., val typ : typ_t [@@column "type"] *)
    let open Ast_pattern in
    let parse_attr (x : attribute) =
      parse
        (pstr (pstr_eval (pexp_constant __) drop ^:: nil))
        x.attr_loc x.attr_payload
      @@ fun v ->
      match (x.attr_name.txt, v) with
      | "column", Pconst_string (v, _, _) -> (`Column, v)
      | _ -> assert false
    in
    let parse_type x =
      let aux : longident -> typ = function
        | Lident "int" -> `Int
        | Lident "string" -> `String
        | Ldot (Lident "Ptime", "t") -> `Ptime
        | Ldot (Ldot (Lident _, "ID"), "t") as s -> `ID s
        | s -> `User s
      in
      match x.pval_type.ptyp_desc with
      | Ptyp_constr ({ txt; _ }, []) -> aux txt
      | Ptyp_constr
          ( { txt = Lident "option"; _ },
            [ { ptyp_desc = Ptyp_constr ({ txt; _ }, _); _ } ] ) ->
          `Option (aux txt)
      | _ -> assert false
    in
    parse (pstr_primitive __) x.pstr_loc x @@ fun x ->
    let name = x.pval_name.txt in
    {
      c_ocaml_name = name;
      c_typ = parse_type x;
      c_sql_name =
        x.pval_attributes |> List.map parse_attr |> List.assoc_opt `Column
        |> Option.value ~default:name;
    }

  let construct_schema ctxt xs =
    let sql_name = ref "" in
    let code_path = Expansion_context.Extension.code_path ctxt in
    let columns =
      ref
        [
          {
            c_ocaml_name = "id";
            c_sql_name = "id";
            c_typ = `Option (`ID (Ldot (lident "ID", "t")));
          };
          {
            c_ocaml_name = "created_at";
            c_sql_name = "created_at";
            c_typ = `Option `Ptime;
          };
          {
            c_ocaml_name = "updated_at";
            c_sql_name = "updated_at";
            c_typ = `Option `Ptime;
          };
        ]
    in
    xs
    |> List.iter (fun x ->
           match parse_config x with
           | `Name name -> sql_name := name
           | exception _ ->
               let column = parse_column x in
               columns := column :: !columns);
    {
      s_sql_name = !sql_name;
      s_columns = List.rev !columns;
      s_code_path = code_path;
    }

  let expand_type_column loc schema =
    let open Ast_builder.Default in
    let rtags =
      schema.s_columns
      |> List.map (fun c -> rtag ~loc { txt = c.c_ocaml_name; loc } true [])
    in
    pstr_type ~loc Nonrecursive
      [
        type_declaration ~loc ~name:{ txt = "column"; loc } ~params:[] ~cstrs:[]
          ~kind:Ptype_abstract ~private_:Public
          ~manifest:(Some (ptyp_variant ~loc rtags Closed None));
      ]

  let expand_let_columns loc schema =
    let open Ast_builder.Default in
    let columns =
      schema.s_columns
      |> List.map (fun c -> pexp_variant ~loc c.c_ocaml_name None)
      |> elist ~loc
    in
    [%stri let columns : column list = [%e columns]]

  let expand_let_string_of_column loc schema =
    let open Ast_builder.Default in
    let cases =
      schema.s_columns
      |> List.map (fun c ->
             case
               ~lhs:(ppat_variant ~loc c.c_ocaml_name None)
               ~guard:None
               ~rhs:(estring ~loc c.c_ocaml_name))
    in
    [%stri
      let string_of_column : column -> string = [%e pexp_function ~loc cases]]

  let expand_type_args loc schema =
    let open Ast_builder.Default in
    let decls =
      schema.s_columns
      |> List.map (fun c ->
             label_declaration ~loc
               ~name:{ txt = c.c_ocaml_name; loc }
               ~mutable_:Immutable
               ~type_:(core_type_of_typ loc c.c_typ))
    in
    pstr_type ~loc Nonrecursive
      [
        type_declaration ~loc ~name:{ txt = "args"; loc } ~params:[] ~cstrs:[]
          ~private_:Public ~manifest:None ~kind:(Ptype_record decls);
      ]

  let expand_class_model loc schema =
    let open Ast_builder.Default in
    let a = gen_symbol ~prefix:"a" () in
    let fields =
      let obj_val name e =
        pcf_val ~loc ({ loc; txt = name }, Mutable, Cfk_concrete (Fresh, e))
      in
      let obj_method name e =
        pcf_method ~loc
          ( { loc; txt = name },
            Public,
            Cfk_concrete (Fresh, pexp_poly ~loc e None) )
      in
      schema.s_columns
      |> List.map (fun c ->
             let name = c.c_ocaml_name in
             let e_name = evar ~loc name in
             let opt = match c.c_typ with `Option _ -> true | _ -> false in
             let wloc txt = { loc; txt } in
             [
               obj_val name (pexp_field ~loc (evar ~loc a) (wloc (lident name)));
               obj_method name
                 (if opt then [%expr Sqlx.Ppx_runtime.expect_loaded [%e e_name]]
                 else e_name);
               obj_method ("set_" ^ name)
                 (let x = gen_symbol () in
                  [%expr
                    fun [%p ppat_var ~loc (wloc x)] ->
                      [%e pexp_setinstvar ~loc (wloc name) (evar ~loc x)]]);
               obj_method ("with_" ^ name)
                 (let x = gen_symbol () in
                  [%expr
                    fun [%p ppat_var ~loc (wloc x)] ->
                      [%e pexp_override ~loc [ (wloc name, evar ~loc x) ]]]);
             ]
             @ if opt then [ obj_method (name ^ "_opt") e_name ] else [])
      |> List.flatten
    in
    pstr_class ~loc
      [
        class_infos ~loc ~virt:Concrete ~params:[] ~name:{ loc; txt = "schema" }
          ~expr:
            (pcl_fun ~loc Nolabel None
               (ppat_var ~loc { loc; txt = a })
               (pcl_structure ~loc
                  (class_structure ~self:(ppat_any ~loc) ~fields)));
      ]

  let expand ~ctxt (xs : structure_item list) =
    let schema = construct_schema ctxt xs in
    Hashtbl.add state
      (Expansion_context.Extension.code_path ctxt
      |> Code_path.fully_qualified_path)
      schema;

    let open Ast_builder.Default in
    let loc = !Ast_helper.default_loc in
    [%stri
      include struct
        module ID : sig
          type t

          val of_int : int -> t
          val to_int : t -> int
        end = struct
          type t = int

          let of_int = Fun.id
          let to_int = Fun.id
        end

        let table_name = [%e estring ~loc schema.s_sql_name]

        [%%i expand_type_column loc schema]
        [%%i expand_let_columns loc schema]
        [%%i expand_let_string_of_column loc schema]
        [%%i expand_type_args loc schema]
        [%%i expand_class_model loc schema]
      end]
end

module Operation = struct
  let expand_let_make loc schema =
    let open Ast_builder.Default in
    let wloc txt = { loc; txt } in
    let xs =
      schema.s_columns
      |> List.map (fun c ->
             (c.c_ocaml_name, gen_symbol ~prefix:c.c_ocaml_name ()))
      |> List.to_seq |> Hashtbl.of_seq
    in
    let body =
      schema.s_columns
      |> List.map (fun c ->
             ( wloc (lident c.c_ocaml_name),
               evar ~loc (Hashtbl.find xs c.c_ocaml_name) ))
    in
    let body = [%expr fun () -> new t [%e pexp_record ~loc body None]] in
    let body =
      schema.s_columns
      |> List.fold_left
           (fun body c ->
             pexp_fun ~loc
               (match c.c_typ with
               | `Option _ -> Optional c.c_ocaml_name
               | _ -> Labelled c.c_ocaml_name)
               None
               (ppat_var ~loc (wloc (Hashtbl.find xs c.c_ocaml_name)))
               body)
           body
    in
    [%stri let make = [%e body]]

  let expand_let_pack loc schema =
    let open Ast_builder.Default in
    let wloc txt = { loc; txt } in
    let x = gen_symbol () in
    let args =
      schema.s_columns
      |> List.map @@ fun c ->
         let decode =
           let decode_id = function
             | Ldot (prefix, _) ->
                 pexp_ident ~loc (wloc (Ldot (prefix, "of_int")))
             | _ -> assert false
           in
           let decode_user = function
             | Lident s -> pexp_ident ~loc (wloc (Lident (s ^ "_of_string")))
             | _ -> assert false
           in
           match (c.c_sql_name, c.c_typ) with
           | "id", _ ->
               [%expr
                 fun x -> x |> Value.expect_int |> ID.of_int |> Option.some]
           | "created_at", _ | "updated_at", _ ->
               [%expr fun x -> x |> Value.expect_timestamp |> Option.some]
           | _, `Int -> [%expr fun x -> x |> Value.expect_int]
           | _, `Option `Int -> [%expr fun x -> x |> Value.expect_int_opt]
           | _, `String -> [%expr fun x -> x |> Value.expect_string]
           | _, `Option `String -> [%expr fun x -> x |> Value.expect_string_opt]
           | _, `Ptime -> [%expr fun x -> x |> Value.expect_timestmap]
           | _, `Option `Ptime ->
               [%expr fun x -> x |> Value.expect_timestamp_opt]
           | _, `ID l ->
               [%expr fun x -> x |> Value.expect_int |> [%e decode_id l]]
           | _, `Option (`ID l) ->
               [%expr
                 fun x -> x |> Value.expect_int |> Option.map [%e decode_id l]]
           | _, `User l ->
               [%expr fun x -> x |> Value.expect_string |> [%e decode_user l]]
           | _, `Option (`User l) ->
               [%expr
                 fun x ->
                   x |> Value.expect_string_opt |> Option.map [%e decode_user l]]
           | _ -> assert false
         in
         let e =
           [%expr
             [%e evar ~loc x]
             |> List.assoc [%e estring ~loc c.c_sql_name]
             |> [%e decode]]
         in
         match c.c_typ with
         | `Option _ -> (Optional c.c_ocaml_name, e)
         | _ -> (Labelled c.c_ocaml_name, e)
    in
    let args = (Nolabel, [%expr ()]) :: args in
    let body = pexp_apply ~loc [%expr make] args in
    [%stri let pack [%p ppat_var ~loc (wloc x)] = [%e body]]

  let expand_let_unpack loc schema =
    let open Ast_builder.Default in
    let wloc txt = { loc; txt } in
    let x = gen_symbol () in
    let body =
      schema.s_columns
      |> List.map @@ fun c ->
         let decode =
           let encode_id = function
             | Ldot (prefix, _) ->
                 pexp_ident ~loc (wloc (Ldot (prefix, "to_int")))
             | _ -> assert false
           in
           let encode_user = function
             | Lident s -> pexp_ident ~loc (wloc (Lident (s ^ "_to_string")))
             | _ -> assert false
           in
           match (c.c_sql_name, c.c_typ) with
           | "id", _ ->
               [%expr fun x -> x |> Option.get |> ID.to_int |> Value.of_int]
           | "created_at", _ | "updated_at", _ ->
               [%expr fun x -> x |> Option.get |> Value.of_timestamp]
           | _, `Int -> [%expr fun x -> x |> Value.of_int]
           | _, `String -> [%expr fun x -> x |> Value.of_string]
           | _, `Ptime -> [%expr fun x -> x |> Value.of_timestamp]
           | _, `ID l -> [%expr fun x -> x |> [%e encode_id l] |> Value.of_int]
           | _, `User l ->
               [%expr fun x -> x |> [%e encode_user l] |> Value.of_string]
           | _, `Option `Int -> [%expr fun x -> x |> Value.of_int_opt]
           | _, `Option `String -> [%expr fun x -> x |> Value.of_string_opt]
           (*
           | _, `Option `Ptime -> [%expr fun x -> x |> Value.of_timestamp_opt]
           *)
           | _, `Option (`ID l) ->
               [%expr
                 fun x -> x |> Option.map [%e encode_id l] |> Value.of_int_opt]
           | _, `Option (`User l) ->
               [%expr
                 fun x ->
                   x |> Option.map [%e encode_user l] |> Value.of_string_opt]
           | _ -> assert false
         in
         pexp_tuple ~loc
           [
             estring ~loc c.c_sql_name;
             pexp_apply ~loc decode
               [
                 ( Nolabel,
                   pexp_send ~loc (evar ~loc x)
                     (wloc
                        (match c.c_typ with
                        | `Option _ -> c.c_ocaml_name ^ "_opt"
                        | _ -> c.c_ocaml_name)) );
               ];
           ]
    in
    let body = elist ~loc body in
    [%stri
      let unpack ([%p ppat_var ~loc (wloc x)] : t) : (string * Value.t) list =
        [%e body]]

  let expand ~ctxt (_xs : structure_item list) =
    let schema =
      Hashtbl.find state
        (Expansion_context.Extension.code_path ctxt
        |> Code_path.fully_qualified_path)
    in
    let loc = !Ast_helper.default_loc in
    [%stri
      include struct
        [%%i expand_let_make loc schema]
        [%%i expand_let_pack loc schema]
        [%%i expand_let_unpack loc schema]
      end]
end

let sqlx_schema =
  Extension.V3.declare "sqlx.schema" Extension.Context.structure_item
    Ast_pattern.(pstr __)
    Schema.expand

let sqlx_gen =
  Extension.V3.declare "sqlx.gen" Extension.Context.structure_item
    Ast_pattern.(pstr __)
    Operation.expand

let () =
  Driver.register_transformation
    ~rules:[ Ppxlib.Context_free.Rule.extension sqlx_schema ]
    "sqlx.schema";
  Driver.register_transformation
    ~rules:[ Ppxlib.Context_free.Rule.extension sqlx_gen ]
    "sqlx.gen";
  ()
