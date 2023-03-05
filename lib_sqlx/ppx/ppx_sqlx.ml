open Ppxlib

type typ =
  [ `Int
  | `String
  | `Ptime
  | `ID of string (* module name *)
  | `User of longident
  | `Option of typ ]

type column = { c_ocaml_name : string; c_sql_name : string; c_typ : typ }

type schema = {
  s_sql_name : string;
  s_columns : column list;
  s_code_path : Code_path.t;
}

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
        | Ldot (Ldot (Lident s, "ID"), "t") -> `ID s
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
    let columns = ref [] in
    let code_path = Expansion_context.Extension.code_path ctxt in
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

  let expand_type_column' loc schema =
    let open Ast_builder.Default in
    let rtags =
      schema.s_columns
      |> List.map (fun c -> rtag ~loc { txt = c.c_ocaml_name; loc } true [])
    in
    pstr_type ~loc Nonrecursive
      [
        type_declaration ~loc ~name:{ txt = "column'"; loc } ~params:[]
          ~cstrs:[] ~kind:Ptype_abstract ~private_:Public
          ~manifest:(Some (ptyp_variant ~loc rtags Closed None));
      ]

  let expand_let_columns loc schema =
    let open Ast_builder.Default in
    let columns =
      schema.s_columns
      |> List.map (fun c -> pexp_variant ~loc c.c_ocaml_name None)
      |> elist ~loc
    in
    [%stri
      let columns : column list =
        `id :: `created_at :: `updated_at :: [%e columns]]

  let expand ~ctxt (xs : structure_item list) =
    let schema = construct_schema ctxt xs in

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

        [%%i expand_type_column' loc schema]

        type column = [ `id | `created_at | `updated_at | column' ]

        [%%i expand_let_columns loc schema]
      end]
end

let expand_gen ~ctxt:_ (_xs : structure_item list) = assert false

let sqlx_schema =
  Extension.V3.declare "sqlx.schema" Extension.Context.structure_item
    Ast_pattern.(pstr __)
    Schema.expand

let sqlx_gen =
  Extension.V3.declare "sqlx.gen" Extension.Context.structure_item
    Ast_pattern.(pstr __)
    expand_gen

let () =
  Driver.register_transformation
    ~rules:[ Ppxlib.Context_free.Rule.extension sqlx_schema ]
    "sqlx.schema";
  Driver.register_transformation
    ~rules:[ Ppxlib.Context_free.Rule.extension sqlx_gen ]
    "sqlx.gen";
  ()
