open Ppxlib
open Ast_builder.Default

let wloc txt =
  let loc = !Ast_helper.default_loc in
  { loc; txt }

type id_ident = { mod_ident : longident option }

type typ =
  [ `Int
  | `String
  | `Ptime
  | `ID of id_ident
  | `User of longident
  | `Option of typ ]

let rec core_type_of_typ loc : typ -> core_type = function
  | `Int -> [%type: int]
  | `String -> [%type: string]
  | `Ptime -> [%type: Ptime.t]
  | `ID { mod_ident = None; _ } ->
      ptyp_constr ~loc { loc; txt = Ldot (Lident "ID", "t") } []
  | `ID { mod_ident = Some m; _ } ->
      ptyp_constr ~loc { loc; txt = Ldot (Ldot (m, "ID"), "t") } []
  | `User l -> ptyp_constr ~loc { loc; txt = l } []
  | `Option t ->
      ptyp_constr ~loc { loc; txt = lident "option" } [ core_type_of_typ loc t ]

let in_mod_ident l s =
  match l.mod_ident with None -> Lident s | Some m -> Ldot (m, s)

let column_name_wo_suffix_id s =
  let open String in
  if ends_with ~suffix:"_id" s then sub s 0 (length s - 3) else assert false

type column = { c_ocaml_name : string; c_sql_name : string; c_typ : typ }
type dummy_column = { d_name : string; d_type : core_type; d_opt : bool }

type schema = {
  s_sql_name : string;
  s_columns : column list;
  s_derived : dummy_column list;
  s_user_defined : dummy_column list;
  s_code_path : Code_path.t;
}

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

let parse_val (x : structure_item) =
  (* Parse one value declaration e.g., val typ : typ_t [@@column "type"] *)
  let open Ast_pattern in
  let parse_attr (x : attribute) =
    match x.attr_name.txt with
    | "not_column" -> `Not_column
    | "column" -> (
        parse
          (pstr (pstr_eval (pexp_constant __) drop ^:: nil))
          x.attr_loc x.attr_payload
        @@ function
        | Pconst_string (v, _, _) -> `Column v
        | _ -> assert false)
    | _ -> assert false
  in
  let parse_type x =
    let aux : longident -> typ = function
      | Lident "int" -> `Int
      | Lident "string" -> `String
      | Ldot (Lident "Ptime", "t") -> `Ptime
      | Ldot (Lident "ID", "t") -> `ID { mod_ident = None }
      | Ldot (Ldot (l, "ID"), "t") -> `ID { mod_ident = Some l }
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
  let attrs = x.pval_attributes |> List.map parse_attr in
  if attrs |> List.mem `Not_column then
    match x.pval_type.ptyp_desc with
    | Ptyp_constr (_, []) ->
        `User_defined { d_name = name; d_type = x.pval_type; d_opt = false }
    | Ptyp_constr ({ txt = Lident "option"; _ }, [ typ ]) ->
        `User_defined { d_name = name; d_type = typ; d_opt = true }
    | _ -> assert false
  else
    `Column
      {
        c_ocaml_name = name;
        c_typ = parse_type x;
        c_sql_name =
          attrs
          |> List.find_map (function `Column s -> Some s | _ -> None)
          |> Option.value ~default:name;
      }

let construct_schema ctxt xs =
  let loc = !Ast_helper.default_loc in
  let sql_name = ref "" in
  let code_path = Expansion_context.Extension.code_path ctxt in
  let columns =
    ref
      [
        {
          c_ocaml_name = "id";
          c_sql_name = "id";
          c_typ = `Option (`ID { mod_ident = None });
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
  let user_defined = ref [] in
  xs
  |> List.iter (fun x ->
         match parse_config x with
         | `Name name -> sql_name := name
         | exception _ -> (
             match parse_val x with
             | `User_defined r -> user_defined := r :: !user_defined
             | `Column column -> columns := column :: !columns));
  let derived =
    !columns
    |> List.filter_map (fun c ->
           match c.c_typ with
           | `ID l ->
               Some
                 {
                   d_type = ptyp_constr ~loc (wloc (in_mod_ident l "t")) [];
                   d_name = column_name_wo_suffix_id c.c_ocaml_name;
                   d_opt = false;
                 }
           | `Option (`ID l) when c.c_ocaml_name <> "id" ->
               Some
                 {
                   d_type = ptyp_constr ~loc (wloc (in_mod_ident l "t")) [];
                   d_name = column_name_wo_suffix_id c.c_ocaml_name;
                   d_opt = true;
                 }
           | _ -> None)
  in
  {
    s_sql_name = !sql_name;
    s_columns = List.rev !columns;
    s_user_defined = !user_defined;
    s_code_path = code_path;
    s_derived = derived;
  }

let expand_type_column loc schema =
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
  let columns =
    schema.s_columns
    |> List.map (fun c -> pexp_variant ~loc c.c_ocaml_name None)
    |> elist ~loc
  in
  [%stri let columns : column list = [%e columns]]

let expand_let_string_of_column loc schema =
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
    let obj_vals_and_methods_for_columns =
      schema.s_columns
      |> List.map @@ fun c ->
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
         @ if opt then [ obj_method (name ^ "_opt") e_name ] else []
    in
    let obj_vals_and_methods_for_derived =
      schema.s_derived @ schema.s_user_defined
      |> List.map @@ fun d ->
         let name = d.d_name in
         let e_name = evar ~loc d.d_name in
         [
           obj_val name [%expr None];
           obj_method ("with_" ^ name)
             (let x = gen_symbol () in
              [%expr
                fun [%p ppat_var ~loc (wloc x)] ->
                  [%e pexp_override ~loc [ (wloc name, evar ~loc x) ]]]);
           obj_method ("set_" ^ name)
             (let x = gen_symbol () in
              [%expr
                fun ([%p ppat_var ~loc (wloc x)] : [%t d.d_type]) ->
                  [%e
                    pexp_setinstvar ~loc (wloc name)
                      [%expr Some [%e evar ~loc x]]]]);
           obj_method name [%expr Sqlx.Ppx_runtime.expect_loaded [%e e_name]];
         ]
         @ if d.d_opt then [ obj_method (name ^ "_opt") e_name ] else []
    in
    obj_vals_and_methods_for_columns @ obj_vals_and_methods_for_derived
    |> List.flatten
  in
  pstr_class ~loc
    [
      class_infos ~loc ~virt:Concrete ~params:[] ~name:{ loc; txt = "t" }
        ~expr:
          (pcl_fun ~loc Nolabel None
             (ppat_var ~loc { loc; txt = a })
             (pcl_structure ~loc
                (class_structure ~self:(ppat_any ~loc) ~fields)));
    ]

let expand_let_make loc schema =
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
  let x = gen_symbol () in
  let args =
    schema.s_columns
    |> List.map @@ fun c ->
       let decode =
         let decode_id l =
           Ldot (in_mod_ident l "ID", "of_int") |> wloc |> pexp_ident ~loc
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
         | _, `String -> [%expr fun x -> x |> Sqlx.Value.expect_string]
         | _, `Ptime -> [%expr fun x -> x |> Sqlx.Value.expect_timestmap]
         | _, `ID l ->
             [%expr fun x -> x |> Sqlx.Value.expect_int |> [%e decode_id l]]
         | _, `User l ->
             [%expr
               fun x -> x |> Sqlx.Value.expect_string |> [%e decode_user l]]
         | _, `Option `Int -> [%expr fun x -> x |> Sqlx.Value.expect_int_opt]
         | _, `Option `String ->
             [%expr fun x -> x |> Sqlx.Value.expect_string_opt]
         | _, `Option `Ptime ->
             [%expr fun x -> x |> Sqlx.Value.expect_timestamp_opt]
         | _, `Option (`ID l) ->
             [%expr
               fun x ->
                 x |> Sqlx.Value.expect_int_opt |> Option.map [%e decode_id l]]
         | _, `Option (`User l) ->
             [%expr
               fun x ->
                 x |> Sqlx.Value.expect_string_opt
                 |> Option.map [%e decode_user l]]
         | _, `Option (`Option _) -> assert false
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
  let x = gen_symbol () in
  let body =
    schema.s_columns
    |> List.map @@ fun c ->
       let encode =
         let encode_id l =
           Ldot (in_mod_ident l "ID", "to_int") |> wloc |> pexp_ident ~loc
         in
         let encode_user = function
           | Lident s -> pexp_ident ~loc (wloc (Lident (s ^ "_to_string")))
           | _ -> assert false
         in
         match c.c_typ with
         | `Int -> [%expr fun x -> x |> Sqlx.Value.of_int]
         | `String -> [%expr fun x -> x |> Sqlx.Value.of_string]
         | `Ptime -> [%expr fun x -> x |> Sqlx.Value.of_timestamp]
         | `ID l -> [%expr fun x -> x |> [%e encode_id l] |> Sqlx.Value.of_int]
         | `User l ->
             [%expr fun x -> x |> [%e encode_user l] |> Sqlx.Value.of_string]
         | `Option `Int -> [%expr fun x -> x |> Sqlx.Value.of_int_opt]
         | `Option `String -> [%expr fun x -> x |> Sqlx.Value.of_string_opt]
         | `Option `Ptime -> [%expr fun x -> x |> Sqlx.Value.of_timestamp_opt]
         | `Option (`ID l) ->
             [%expr
               fun x ->
                 x |> Option.map [%e encode_id l] |> Sqlx.Value.of_int_opt]
         | `Option (`User l) ->
             [%expr
               fun x ->
                 x |> Option.map [%e encode_user l] |> Sqlx.Value.of_string_opt]
         | `Option (`Option _) -> assert false
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
    let unpack ([%p ppat_var ~loc (wloc x)] : t) : (string * Sqlx.Value.t) list
        =
      [%e body]]

let expand_let_load_column loc col =
  let opt, id_t =
    match col.c_typ with
    | `ID l -> (false, l)
    | `Option (`ID l) -> (true, l)
    | _ -> assert false
  in
  let column_wo_id = column_name_wo_suffix_id col.c_ocaml_name in
  let funname = "load_" ^ column_wo_id in
  let select = in_mod_ident id_t "select" |> wloc |> pexp_ident ~loc in
  let x_set_column =
    (* e.g., x#set_account *)
    pexp_send ~loc [%expr x] (wloc ("set_" ^ column_wo_id))
  in
  let x_column_id =
    (* e.g., x#account_id *)
    pexp_send ~loc [%expr x] (wloc col.c_ocaml_name)
  in
  let x_column_id_opt =
    (* e.g., x#account_id *)
    pexp_send ~loc [%expr x] (wloc (col.c_ocaml_name ^ "_opt"))
  in
  value_binding ~loc
    ~pat:(ppat_var ~loc (wloc funname))
    ~expr:
      [%expr
        fun (xs : t list) (c : Sqlx.Ppx_runtime.connection) ->
          let ids =
            [%e
              if opt then
                [%expr xs |> List.filter_map (fun x -> [%e x_column_id_opt])]
              else [%expr xs |> List.map (fun x -> [%e x_column_id])]]
          in
          match ids with
          | [] ->
              (* Prevent casting useless 'WHERE FALSE' queries *)
              Lwt.return_unit
          | _ ->
              let ( >|= ) x f = Lwt.map f x in
              [%e select] ~id:(`In ids) c
              >|= Sqlx.Ppx_runtime.index_by (fun y -> y#id)
              >|= fun tbl ->
              xs
              |> List.iter @@ fun x ->
                 [%e
                   if opt then
                     [%expr
                       match [%e x_column_id_opt] with
                       | None -> ()
                       | Some y -> [%e x_set_column] (Hashtbl.find tbl y)]
                   else
                     [%expr
                       [%e x_set_column] (Hashtbl.find tbl [%e x_column_id])]]]

let preload_info_of_schema loc schema =
  let preload_spec_src =
    schema.s_columns
    |> List.filter_map @@ fun c ->
       match c.c_typ with
       | (`ID _ | `Option (`ID _)) when c.c_ocaml_name <> "id" ->
           let column_wo_id = column_name_wo_suffix_id c.c_ocaml_name in
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
  (preload_spec, preload_all)

let expand_where loc schema where p =
  schema.s_columns
  |> List.fold_left
       (fun body c ->
         match c.c_ocaml_name with
         | "id" | "created_at" | "updated_at" -> body
         | _ ->
             let ident = pexp_ident ~loc (wloc (lident c.c_ocaml_name)) in
             let estr = estring ~loc c.c_sql_name in
             let where_id l =
               in_mod_ident l "where_id" |> wloc |> pexp_ident ~loc
             in
             let where_id_opt l =
               in_mod_ident l "where_id_opt" |> wloc |> pexp_ident ~loc
             in
             let encode_user = function
               | Lident s -> pexp_ident ~loc (wloc (Lident (s ^ "_to_string")))
               | _ -> assert false
             in
             let where =
               match c.c_typ with
               | `Int -> [%expr Sqlx.Sql.where_int]
               | `String -> [%expr Sqlx.Sql.where_string ~encode:Fun.id]
               | `Ptime -> [%expr Sqlx.Sql.where_timestamp]
               | `ID l -> where_id l
               | `User l ->
                   [%expr Sqlx.Sql.where_string ~encode:[%e encode_user l]]
               | `Option `Int -> [%expr Sqlx.Sql.where_int_opt]
               | `Option `String ->
                   [%expr Sqlx.Sql.where_string_opt ~encode:Fun.id]
               | `Option `Ptime -> [%expr Sqlx.Sql.where_timestamp_opt]
               | `Option (`ID l) -> where_id_opt l
               | `Option (`User l) ->
                   [%expr Sqlx.Sql.where_string_opt ~encode:[%e encode_user l]]
               | _ -> assert false
             in
             [%expr [%e where] [%e estr] [%e ident] [%e body]])
       [%expr
         let sym_tbl = Hashtbl.create 0 in
         let parse x =
           let rec f = function
             | #column as c -> `C (string_of_column c)
             | `Or (e1, e2) -> `Or (f e1, f e2)
             | `And (e1, e2) -> `And (f e1, f e2)
             | `Eq (e1, e2) -> `Eq (f e1, f e2)
             | `IsNull (#column as c) -> `IsNull (string_of_column c)
             | `IsNotNull (#column as c) -> `IsNotNull (string_of_column c)
             | `Raw s -> `Raw s
             | k -> (
                 match Hashtbl.find_opt sym_tbl k with
                 | Some x -> `UM x
                 | None ->
                     let sym = Hashtbl.length sym_tbl |> string_of_int in
                     Hashtbl.add sym_tbl k sym;
                     `UM sym)
           in
           [ f x ]
         in
         let where = [%e where] |> Option.fold ~none:[] ~some:parse in
         let p =
           [%e p] |> List.map (fun (k, v) -> (`UM (Hashtbl.find sym_tbl k), v))
         in
         (where, p)]

let expand_optionally_labelled_columns loc schema body =
  schema.s_columns
  |> List.fold_left
       (fun body c ->
         pexp_fun ~loc (Optional c.c_ocaml_name) None
           (ppat_var ~loc (wloc c.c_ocaml_name))
           body)
       body

let expand_let_select loc schema =
  let preload_spec, preload_all = preload_info_of_schema loc schema in
  value_binding ~loc
    ~pat:(ppat_var ~loc (wloc "select"))
    ~expr:
      (expand_optionally_labelled_columns loc schema
         [%expr
           fun ?where ?(p = []) ?order_by ?limit ?(preload = [%e preload_all]) c ->
             select' id created_at updated_at order_by limit preload c
               [%e preload_spec]
               [%e expand_where loc schema [%expr where] [%expr p]]])

let expand_let_select_and_load_columns loc schema =
  schema.s_columns
  |> List.filter_map (fun c ->
         match c.c_typ with
         | (`ID _ | `Option (`ID _)) when c.c_ocaml_name <> "id" ->
             Some (expand_let_load_column loc c)
         | _ -> None)
  |> List.cons (expand_let_select loc schema)
  |> pstr_value ~loc Recursive

let expand_let_update loc schema =
  let preload_spec, preload_all = preload_info_of_schema loc schema in
  [%stri
    let update ?(preload = [%e preload_all]) xs c =
      update xs c preload [%e preload_spec]]

let expand_let_insert loc schema =
  let preload_spec, preload_all = preload_info_of_schema loc schema in
  [%stri
    let insert ?(preload = [%e preload_all]) xs c =
      insert xs c preload [%e preload_spec]]

let expand_let_count loc schema =
  [%stri
    let count =
      [%e
        expand_optionally_labelled_columns loc schema
          [%expr
            fun ?where ?(p = []) c ->
              count id created_at updated_at c
                [%e expand_where loc schema [%expr where] [%expr p]]]]]

let expand_let_all_derived_columns loc schema =
  let _preload_spec, preload_all = preload_info_of_schema loc schema in
  [%stri let all_derived_columns = [%e preload_all]]

let expand_let_get_many loc schema =
  [%stri
    let get_many =
      [%e
        expand_optionally_labelled_columns loc schema
          [%expr
            fun ?where ?(p = []) ?order_by ?limit
                ?(preload = all_derived_columns) c ->
              [%e
                pexp_apply ~loc [%expr select]
                  (schema.s_columns
                  |> List.map @@ fun c ->
                     ( Optional c.c_ocaml_name,
                       [%expr
                         [%e pexp_ident ~loc (wloc (lident c.c_ocaml_name))]
                         |> Option.map (fun x -> `Eq x)] ))]
                ?where ?order_by ?limit ~p ~preload c]]]

let expand_let_get_one loc schema =
  [%stri
    let get_one =
      [%e
        expand_optionally_labelled_columns loc schema
          [%expr
            fun ?where ?(p = []) ?order_by ?(preload = all_derived_columns) c ->
              [%e
                pexp_apply ~loc [%expr get_many]
                  (schema.s_columns
                  |> List.map @@ fun c ->
                     ( Optional c.c_ocaml_name,
                       pexp_ident ~loc (wloc (lident c.c_ocaml_name)) ))]
                ~limit:1 ?where ?order_by ~p ~preload c
              |> Lwt.map Sqlx.Ppx_runtime.expect_single_row]]]

let expand ~ctxt (xs : structure_item list) =
  let loc = !Ast_helper.default_loc in
  let schema = construct_schema ctxt xs in
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
      [%%i expand_let_all_derived_columns loc schema]
      [%%i expand_type_args loc schema]
      [%%i expand_class_model loc schema]
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
      [%%i expand_let_update loc schema]
      [%%i expand_let_insert loc schema]
      [%%i expand_let_count loc schema]
      [%%i expand_let_get_many loc schema]
      [%%i expand_let_get_one loc schema]

      let save_one ?(preload = all_derived_columns) (x : t) c =
        match x#id_opt with
        | None -> insert ~preload [ x ] c |> Lwt.map List.hd
        | Some _ -> update ~preload [ x ] c |> Lwt.map List.hd
    end]

let sqlx_schema =
  Extension.V3.declare "sqlx.schema" Extension.Context.structure_item
    Ast_pattern.(pstr __)
    expand

let () =
  Driver.register_transformation
    ~rules:[ Ppxlib.Context_free.Rule.extension sqlx_schema ]
    "sqlx.schema";
  ()
