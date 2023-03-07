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
type derived_column = { d_name : string; d_type : core_type }

type schema = {
  s_sql_name : string;
  s_columns : column list;
  s_derived : derived_column list;
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
    let open Ast_builder.Default in
    let loc = !Ast_helper.default_loc in
    let wloc txt = { loc; txt } in
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
    let derived =
      !columns
      |> List.filter_map (fun c ->
             match c.c_typ with
             | `ID (Ldot (Ldot (l, "ID"), "t")) ->
                 let d_type = ptyp_constr ~loc (wloc (Ldot (l, "t"))) [] in
                 let d_name =
                   let s = c.c_ocaml_name in
                   let open String in
                   if ends_with ~suffix:"_id" s then sub s 0 (length s - 3)
                   else assert false
                 in
                 Some { d_type; d_name }
             | _ -> None)
    in
    {
      s_sql_name = !sql_name;
      s_columns = List.rev !columns;
      s_code_path = code_path;
      s_derived = derived;
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
               ~rhs:(estring ~loc c.c_sql_name))
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
    let wloc txt = { loc; txt } in
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
      (schema.s_columns
      |> List.map (fun c ->
             let name = c.c_ocaml_name in
             let e_name = evar ~loc name in
             let opt = match c.c_typ with `Option _ -> true | _ -> false in
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
      |> List.flatten)
      @ (schema.s_derived
        |> List.map (fun d ->
               let name = d.d_name in
               let e_name = evar ~loc d.d_name in
               [
                 obj_val name [%expr None];
                 obj_method ("set_" ^ name)
                   (let x = gen_symbol () in
                    [%expr
                      fun ([%p ppat_var ~loc (wloc x)] : [%t d.d_type]) ->
                        [%e
                          pexp_setinstvar ~loc (wloc name)
                            [%expr Some [%e evar ~loc x]]]]);
                 obj_method name
                   [%expr Sqlx.Ppx_runtime.expect_loaded [%e e_name]];
               ])
        |> List.flatten)
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
                 fun x -> x |> Sqlx.Value.expect_int |> ID.of_int |> Option.some]
           | "created_at", _ | "updated_at", _ ->
               [%expr fun x -> x |> Sqlx.Value.expect_timestamp |> Option.some]
           | _, `Int -> [%expr fun x -> x |> Sqlx.Value.expect_int]
           | _, `Option `Int -> [%expr fun x -> x |> Sqlx.Value.expect_int_opt]
           | _, `String -> [%expr fun x -> x |> Sqlx.Value.expect_string]
           | _, `Option `String ->
               [%expr fun x -> x |> Sqlx.Value.expect_string_opt]
           | _, `Ptime -> [%expr fun x -> x |> Sqlx.Value.expect_timestmap]
           | _, `Option `Ptime ->
               [%expr fun x -> x |> Sqlx.Value.expect_timestamp_opt]
           | _, `ID l ->
               [%expr fun x -> x |> Sqlx.Value.expect_int |> [%e decode_id l]]
           | _, `Option (`ID l) ->
               [%expr
                 fun x ->
                   x |> Sqlx.Value.expect_int |> Option.map [%e decode_id l]]
           | _, `User l ->
               [%expr
                 fun x -> x |> Sqlx.Value.expect_string |> [%e decode_user l]]
           | _, `Option (`User l) ->
               [%expr
                 fun x ->
                   x |> Sqlx.Value.expect_string_opt
                   |> Option.map [%e decode_user l]]
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
         let encode =
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
           | _, `Int -> [%expr fun x -> x |> Sqlx.Value.of_int]
           | _, `String -> [%expr fun x -> x |> Sqlx.Value.of_string]
           | _, `Ptime -> [%expr fun x -> x |> Sqlx.Value.of_timestamp]
           | _, `ID l ->
               [%expr fun x -> x |> [%e encode_id l] |> Sqlx.Value.of_int]
           | _, `User l ->
               [%expr fun x -> x |> [%e encode_user l] |> Sqlx.Value.of_string]
           | _, `Option `Int -> [%expr fun x -> x |> Sqlx.Value.of_int_opt]
           | _, `Option `String ->
               [%expr fun x -> x |> Sqlx.Value.of_string_opt]
           | _, `Option `Ptime ->
               [%expr fun x -> x |> Sqlx.Value.of_timestamp_opt]
           | _, `Option (`ID l) ->
               [%expr
                 fun x ->
                   x |> Option.map [%e encode_id l] |> Sqlx.Value.of_int_opt]
           | _, `Option (`User l) ->
               [%expr
                 fun x ->
                   x
                   |> Option.map [%e encode_user l]
                   |> Sqlx.Value.of_string_opt]
           | _ -> assert false
         in
         pexp_tuple ~loc
           [
             estring ~loc c.c_sql_name;
             pexp_apply ~loc encode
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
      let unpack ([%p ppat_var ~loc (wloc x)] : t) :
          (string * Sqlx.Value.t) list =
        [%e body]]

  let expand_let_load_column loc col =
    let open Ast_builder.Default in
    let wloc txt = { loc; txt } in
    let id_t = match col.c_typ with `ID l -> l | _ -> assert false in
    let column_wo_id =
      let s = col.c_ocaml_name in
      let open String in
      if ends_with ~suffix:"_id" s then sub s 0 (length s - 3) else assert false
    in
    let funname = "load_" ^ column_wo_id in
    let select =
      match id_t with
      | Ldot (Ldot (l, "ID"), "t") ->
          pexp_ident ~loc (wloc (Ldot (l, "select")))
      | _ -> assert false
    in
    let x_column_id =
      (* e.g., x#account_id *)
      pexp_send ~loc [%expr x] (wloc col.c_ocaml_name)
    in
    let x_set_column =
      (* e.g., x#set_account *)
      pexp_send ~loc [%expr x] (wloc ("set_" ^ column_wo_id))
    in
    value_binding ~loc
      ~pat:(ppat_var ~loc (wloc funname))
      ~expr:
        [%expr
          fun (xs : t list) (c : Sqlx.Ppx_runtime.connection) ->
            let ids = xs |> List.map (fun x -> [%e x_column_id]) in
            Lwt.map
              (fun tbl ->
                xs
                |> List.iter (fun x ->
                       Hashtbl.find tbl [%e x_column_id] |> [%e x_set_column]))
              (Lwt.map (index_by (fun y -> y#id)) ([%e select] ~id:(`In ids) c))]

  let expand_let_select loc schema =
    let open Ast_builder.Default in
    let wloc txt = { loc; txt } in
    let body = [%expr [], []] in
    let body =
      schema.s_columns
      |> List.fold_left
           (fun body c ->
             match c.c_ocaml_name with
             | "id" | "created_at" | "updated_at" -> body
             | _ ->
                 let ident = pexp_ident ~loc (wloc (lident c.c_ocaml_name)) in
                 let estr = estring ~loc c.c_sql_name in
                 let where_id = function
                   | Ldot (Ldot (l, "ID"), "t") ->
                       pexp_ident ~loc (wloc (Ldot (l, "where_id")))
                   | _ -> assert false
                 in
                 let encode_user = function
                   | Lident s ->
                       pexp_ident ~loc (wloc (Lident (s ^ "_to_string")))
                   | _ -> assert false
                 in
                 let where =
                   match c.c_typ with
                   | `Int -> [%expr Sqlx.Sql.where_int]
                   | `String -> [%expr Sqlx.Sql.where_string]
                   | `Ptime -> [%expr Sqlx.Sql.where_timestamp]
                   | `ID l -> where_id l
                   | `User l ->
                       [%expr Sqlx.Sql.where_string ~encode:[%e encode_user l]]
                   | `Option `Int -> [%expr Sqlx.Sql.where_int_opt]
                   | `Option `String -> [%expr Sqlx.Sql.where_string_opt]
                   | `Option `Ptime -> [%expr Sqlx.Sql.where_timestamp_opt]
                   | `Option (`ID _l) -> assert false
                   | `Option (`User l) ->
                       [%expr
                         Sqlx.Sql.where_string_opt ~encode:[%e encode_user l]]
                   | _ -> assert false
                 in
                 [%expr [%e where] [%e estr] [%e ident] [%e body]])
           body
    in
    let preload_spec_src =
      schema.s_columns
      |> List.filter_map @@ fun c ->
         match c.c_typ with
         | `ID _ ->
             let column_wo_id =
               let s = c.c_ocaml_name in
               let open String in
               if ends_with ~suffix:"_id" s then sub s 0 (length s - 3)
               else assert false
             in
             Some
               ( pexp_variant ~loc column_wo_id None,
                 pexp_ident ~loc (wloc (lident ("load_" ^ column_wo_id))) )
         | _ -> None
    in
    let preload_all = preload_spec_src |> List.map fst |> elist ~loc in
    let preload_spec =
      preload_spec_src
      |> List.map (fun (v, f) -> pexp_tuple ~loc [ v; f ])
      |> elist ~loc
    in
    let body =
      [%expr
        select' id created_at updated_at order_by limit preload c
          [%e preload_spec] [%e body]]
    in
    value_binding ~loc
      ~pat:(ppat_var ~loc (wloc "select"))
      ~expr:
        (schema.s_columns
        |> List.fold_left
             (fun body c ->
               pexp_fun ~loc (Optional c.c_ocaml_name) None
                 (ppat_var ~loc (wloc c.c_ocaml_name))
                 body)
             [%expr
               fun ?order_by ?limit ?(preload = [%e preload_all]) c -> [%e body]]
        )

  let expand_let_select_and_load_columns loc schema =
    let open Ast_builder.Default in
    schema.s_columns
    |> List.filter_map (fun c ->
           match c.c_typ with
           | `ID _ -> Some (expand_let_load_column loc c)
           | _ -> None)
    |> List.cons (expand_let_select loc schema)
    |> pstr_value ~loc Recursive

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

        let id x = x#id

        let after_create_commit_callbacks :
            (t -> Sqlx.Ppx_runtime.connection -> unit Lwt.t) list ref =
          ref []

        include Sqlx.Ppx_runtime.Make (struct
          module ID = ID

          (* Avoid cyclic definitions *)
          type t' = t
          type t = t'
          type column' = column
          type column = column'

          let columns = columns
          let string_of_column = string_of_column
          let table_name = table_name
          let unpack = unpack
          let pack = pack
          let id = id
          let after_create_commit_callbacks = after_create_commit_callbacks
        end)

        let select' = select

        [%%i expand_let_select_and_load_columns loc schema]
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
