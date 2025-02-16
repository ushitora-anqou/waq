open Ppxlib
open Ast_builder.Default

type id_ident = { mod_ident : longident option }
type expand_env = { mutable e_mod_name : (string, string) Hashtbl.t }

let init_expand_env = { e_mod_name = Hashtbl.create 1 }
let env_add_mod env key name = Hashtbl.add env.e_mod_name key name
let env_get_mod_opt env key = Hashtbl.find_opt env.e_mod_name key
let env_get_mod env key = env_get_mod_opt env key |> Option.get

let env_replace_mod_name env ty =
  (* Convert e.g., Status.t ==> Status_001_.t *)
  let rec aux ty =
    match ty.ptyp_desc with
    | Ptyp_constr (body, args) ->
        let body =
          match body with
          | { txt = Ldot (Lident ident, ("t" as key)); loc } ->
              let txt =
                Ldot
                  ( Lident
                      (env_get_mod_opt env ident |> Option.value ~default:ident),
                    key )
              in
              { txt; loc }
          | { txt = Ldot (Ldot (Lident ident, "ID"), "t"); loc } ->
              let txt =
                Ldot
                  ( Ldot
                      ( Lident
                          (env_get_mod_opt env ident
                          |> Option.value ~default:ident),
                        "ID" ),
                    "t" )
              in
              { txt; loc }
          | _ -> body
        in
        { ty with ptyp_desc = Ptyp_constr (body, List.map aux args) }
    | _ -> ty
  in
  aux ty

let env_get_mod_ident env x =
  match x.mod_ident with
  | Some (Lident name) -> { mod_ident = Some (Lident (env_get_mod env name)) }
  | _ -> x

let env_open_mod_in_expr env loc name =
  let mod_name = env_get_mod env name in
  pexp_open ~loc
    (open_infos ~loc ~override:Fresh
       ~expr:(pmod_ident ~loc { loc; txt = Lident mod_name }))

let wloc txt =
  let loc = !Ast_helper.default_loc in
  { loc; txt }

let empty_variant_type ~loc name =
  (* [%stri type name = | ] *)
  pstr_type ~loc Recursive
    [
      type_declaration ~loc ~name:(wloc name) ~params:[] ~cstrs:[]
        ~kind:(Ptype_variant []) ~private_:Public ~manifest:None;
    ]

type typ =
  [ `Int
  | `String
  | `Ptime
  | `Bool
  | `ID of id_ident
  | `User of [ `Int | `String ] (* underlying_type *) * longident (* type *)
  | `Option of typ ]

let rec core_type_of_type loc : typ -> core_type = function
  | `Int -> [%type: int]
  | `String -> [%type: string]
  | `Ptime -> [%type: Ptime.t]
  | `Bool -> [%type: bool]
  | `ID { mod_ident = None; _ } ->
      ptyp_constr ~loc { loc; txt = Ldot (Lident "ID", "t") } []
  | `ID { mod_ident = Some m; _ } ->
      ptyp_constr ~loc { loc; txt = Ldot (Ldot (m, "ID"), "t") } []
  | `User (_, l) -> ptyp_constr ~loc { loc; txt = l } []
  | `Option t ->
      ptyp_constr ~loc
        { loc; txt = lident "option" }
        [ core_type_of_type loc t ]

let in_mod_ident l s =
  match l.mod_ident with None -> Lident s | Some m -> Ldot (m, s)

let column_name_wo_suffix_id s =
  let open String in
  if ends_with ~suffix:"_id" s then sub s 0 (length s - 3) else assert false

type column_body = { c_ocaml_name : string; c_sql_name : string; c_type : typ }
type column = CID | CCreatedAt | CUpdatedAt | CNormal of column_body
type derived_column = { d_name : string; d_id_ident : id_ident; d_opt : bool }

type user_defined_field = {
  u_name : string;
  u_core_type : core_type;
  u_preload_spec : core_type option;
}

type related_field = {
  r_name : string;
  r_relation :
    [ `HasOneOrZero (* M.t option *)
    | `HasOne (* M.t *)
    | `HasMany (* M.t list *) ];
  r_foregin_mod_name : string;
  r_foregin_key : string;
  r_is_foregin_key_opt : bool;
}

type schema = {
  s_ocaml_mod_name : string;
  s_sql_name : string;
  s_columns : column list;
  s_derived : derived_column list;
  s_user_defined : user_defined_field list;
  s_code_path : Code_path.t;
  s_custom_header : structure_item list;
  s_related_field : related_field list;
}

let loadable_fields_of_schema schema =
  (schema.s_derived |> List.map (fun f -> f.d_name))
  @ (schema.s_user_defined |> List.map (fun f -> f.u_name))

let ocaml_name_of_column = function
  | CID -> "id"
  | CCreatedAt -> "created_at"
  | CUpdatedAt -> "updated_at"
  | CNormal { c_ocaml_name; _ } -> c_ocaml_name

let sql_name_of_column = function
  | CID -> "id"
  | CCreatedAt -> "created_at"
  | CUpdatedAt -> "updated_at"
  | CNormal { c_sql_name; _ } -> c_sql_name

let type_of_column = function
  | CID -> `Option (`ID { mod_ident = None })
  | CCreatedAt | CUpdatedAt -> `Option `Ptime
  | CNormal { c_type; _ } -> c_type

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

let parse_item (x : structure_item) =
  let open Ast_pattern in
  let parse_attr (x : attribute) =
    match x.attr_name.txt with
    | "not_column" -> `Not_column
    | "foreign_key" ->
        parse
          (pstr (pstr_eval (pexp_variant __ none) drop ^:: nil))
          x.attr_loc x.attr_payload
          (fun v -> `Foreign_key v)
    | "preload_spec" ->
        parse (ptyp __) x.attr_loc x.attr_payload @@ fun l -> `Preload_spec l
    | "underlying_type" ->
        parse (ptyp __) x.attr_loc x.attr_payload @@ fun l -> `Underlying_type l
    | "column" -> (
        parse
          (pstr (pstr_eval (pexp_constant __) drop ^:: nil))
          x.attr_loc x.attr_payload
        @@ function
        | Pconst_string (v, _, _) -> `Column v
        | _ -> assert false)
    | _ -> assert false
  in
  let parse_type (underlying_type : core_type option) x =
    let aux : longident -> typ = function
      | Lident "int" -> `Int
      | Lident "string" -> `String
      | Lident "bool" -> `Bool
      | Ldot (Lident "Ptime", "t") -> `Ptime
      | Ldot (Lident "ID", "t") -> `ID { mod_ident = None }
      | Ldot (Ldot (l, "ID"), "t") -> `ID { mod_ident = Some l }
      | id ->
          let underlying_type =
            underlying_type
            |> Option.fold ~none:`String ~some:(fun ty ->
                   match ty.ptyp_desc with
                   | Ptyp_constr ({ txt = Lident "int"; _ }, []) -> `Int
                   | Ptyp_constr ({ txt = Lident "string"; _ }, []) -> `String
                   | _ -> assert false)
          in
          `User (underlying_type, id)
    in
    match x.ptyp_desc with
    | Ptyp_constr ({ txt; _ }, []) -> aux txt
    | Ptyp_constr
        ( { txt = Lident "option"; _ },
          [ { ptyp_desc = Ptyp_constr ({ txt; _ }, _); _ } ] ) ->
        `Option (aux txt)
    | _ -> assert false
  in
  let parse_class_type xs =
    xs
    |> List.map @@ fun x ->
       let attrs = x.pctf_attributes |> List.map parse_attr in
       let name, typ =
         match x.pctf_desc with
         | Pctf_val (name, _, _, typ) -> (name.txt, typ)
         | _ -> assert false
       in
       if attrs |> List.mem `Not_column then
         match typ.ptyp_desc with
         | Ptyp_constr _ ->
             `User_defined
               {
                 u_name = name;
                 u_core_type = typ;
                 u_preload_spec =
                   attrs
                   |> List.find_map (function
                        | `Preload_spec l -> Some l
                        | _ -> None);
               }
         | _ -> assert false
       else
         match
           attrs
           |> List.find_map @@ function `Foreign_key s -> Some s | _ -> None
         with
         | Some foreign_key ->
             let r_relation, r_foregin_mod_name =
               let parse_M_t = function
                 | {
                     ptyp_desc =
                       Ptyp_constr ({ txt = Ldot (Lident s, "t"); _ }, []);
                     _;
                   } ->
                     Some s
                 | _ -> None
               in
               match parse_M_t typ with
               | Some s -> (`HasOne, s)
               | None -> (
                   match typ.ptyp_desc with
                   | Ptyp_constr ({ txt = Lident "list"; _ }, [ arg ]) ->
                       (`HasMany, Option.get (parse_M_t arg))
                   | Ptyp_constr ({ txt = Lident "option"; _ }, [ arg ]) ->
                       (`HasOneOrZero, Option.get (parse_M_t arg))
                   | _ -> assert false)
             in
             `Related_field
               {
                 r_name = name;
                 r_relation;
                 r_foregin_mod_name;
                 r_foregin_key = foreign_key;
                 r_is_foregin_key_opt = false (* dummy *);
               }
         | None ->
             let underlying_type =
               attrs
               |> List.find_map (function
                    | `Underlying_type s -> Some s
                    | _ -> None)
             in
             `Column
               {
                 c_ocaml_name = name;
                 c_type = parse_type underlying_type typ;
                 c_sql_name =
                   attrs
                   |> List.find_map (function `Column s -> Some s | _ -> None)
                   |> Option.value ~default:name;
               }
  in
  try
    (* Parse `class type t = object val xxx : yyy ... end` *)
    parse
      (pstr_class_type
         (class_infos ~virt:concrete ~params:nil ~name:(string "t")
            ~expr:
              (pcty_signature (class_signature ~self:drop ~fields:(many __)))
         ^:: nil))
      x.pstr_loc x
      (fun xs -> `Columns (parse_class_type xs))
  with _ -> `NotField x

let construct_schema ctxt xs =
  let sql_name = ref "" in
  let code_path = Expansion_context.Extension.code_path ctxt in
  let columns = ref [ CID; CCreatedAt; CUpdatedAt ] in
  let user_defined = ref [] in
  let not_field = ref [] in
  let related_field = ref [] in
  xs
  |> List.iter (fun x ->
         match parse_config x with
         | `Name name -> sql_name := name
         | exception _ -> (
             match parse_item x with
             | `NotField x -> not_field := x :: !not_field
             | `Columns xs -> (
                 xs
                 |> List.iter @@ function
                    | `Related_field r -> related_field := r :: !related_field
                    | `User_defined r -> user_defined := r :: !user_defined
                    | `Column column -> columns := CNormal column :: !columns)));
  let derived =
    !columns
    |> List.filter_map @@ function
       | CNormal { c_type = `ID l; c_ocaml_name; _ } ->
           Some
             {
               d_name = column_name_wo_suffix_id c_ocaml_name;
               d_id_ident = l;
               d_opt = false;
             }
       | CNormal { c_type = `Option (`ID l); c_ocaml_name; _ } ->
           Some
             {
               d_name = column_name_wo_suffix_id c_ocaml_name;
               d_id_ident = l;
               d_opt = true;
             }
       | _ -> None
  in
  {
    s_ocaml_mod_name = "";
    s_sql_name = !sql_name;
    s_columns = List.rev !columns;
    s_user_defined = !user_defined;
    s_code_path = code_path;
    s_derived = derived;
    s_custom_header = List.rev !not_field;
    s_related_field = !related_field;
  }

let analyze_schemas (schemas : schema list) =
  let schemas =
    schemas
    |> List.map @@ fun schema ->
       let s_related_field =
         schema.s_related_field
         |> List.map @@ fun rel ->
            let schema' =
              schemas
              |> List.find (fun s ->
                     s.s_ocaml_mod_name = rel.r_foregin_mod_name)
            in
            let foregin =
              schema'.s_columns
              |> List.find_map (function
                   | CNormal c when c.c_ocaml_name = rel.r_foregin_key -> Some c
                   | _ -> None)
              |> Option.get
            in
            match foregin.c_type with
            | `Option (`ID _) -> { rel with r_is_foregin_key_opt = true }
            | `ID _ -> { rel with r_is_foregin_key_opt = false }
            | _ -> assert false
       in
       let s_user_defined =
         schema.s_user_defined
         |> List.map @@ fun u ->
            match u.u_preload_spec with
            | Some
                ({
                   ptyp_desc =
                     Ptyp_constr ({ txt = Lident "preload_spec"; loc }, args);
                   _;
                 } as ty) ->
                {
                  u with
                  u_preload_spec =
                    Some
                      {
                        ty with
                        ptyp_desc =
                          Ptyp_constr
                            ( {
                                txt =
                                  Ldot
                                    ( Lident schema.s_ocaml_mod_name,
                                      "preload_spec" );
                                loc;
                              },
                              args );
                      };
                }
            | _ -> u
       in
       { schema with s_related_field; s_user_defined }
  in
  schemas

let expand_type_column loc schema =
  let rtags =
    schema.s_columns
    |> List.map (fun c ->
           rtag ~loc { txt = ocaml_name_of_column c; loc } true [])
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
    |> List.map (fun c -> pexp_variant ~loc (ocaml_name_of_column c) None)
    |> elist ~loc
  in
  [%stri let columns : column list = [%e columns]]

let expand_let_string_of_column loc schema =
  let cases =
    schema.s_columns
    |> List.map (fun c ->
           case
             ~lhs:(ppat_variant ~loc (ocaml_name_of_column c) None)
             ~guard:None
             ~rhs:(estring ~loc (sql_name_of_column c)))
  in
  [%stri
    let string_of_column : column -> string = [%e pexp_function ~loc cases]]

let expand_type_args env loc schema =
  let decls =
    schema.s_columns
    |> List.map (fun c ->
           label_declaration ~loc
             ~name:{ txt = ocaml_name_of_column c; loc }
             ~mutable_:Immutable
             ~type_:
               (type_of_column c |> core_type_of_type loc
              |> env_replace_mod_name env))
  in
  pstr_type ~loc Nonrecursive
    [
      type_declaration ~loc ~name:{ txt = "args"; loc } ~params:[] ~cstrs:[]
        ~private_:Public ~manifest:None ~kind:(Ptype_record decls);
    ]

let expand_class_t_fields env loc schema a replace_rec_type =
  let obj_val name e =
    pcf_val ~loc ({ loc; txt = name }, Mutable, Cfk_concrete (Fresh, e))
  in
  let obj_method name e =
    pcf_method ~loc
      ({ loc; txt = name }, Public, Cfk_concrete (Fresh, pexp_poly ~loc e None))
  in
  let obj_method_with loc name =
    let x = gen_symbol () in
    obj_method ("with_" ^ name)
      [%expr
        fun [%p ppat_var ~loc (wloc x)] ->
          [%e pexp_override ~loc [ (wloc name, evar ~loc x) ]]]
  in
  let obj_method_set loc name =
    let x = gen_symbol () in
    obj_method ("set_" ^ name)
      [%expr
        fun [%p ppat_var ~loc (wloc x)] ->
          [%e pexp_setinstvar ~loc (wloc name) (evar ~loc x)]]
  in
  let obj_vals_and_methods_for_columns =
    schema.s_columns
    |> List.map @@ function
       | CID ->
           [
             obj_val "id" [%expr [%e evar ~loc a].id];
             obj_method "id" [%expr Sqlx.Ppx_runtime.expect_loaded id];
             obj_method "id_opt" [%expr id];
           ]
       | CCreatedAt ->
           [
             obj_val "created_at" [%expr [%e evar ~loc a].created_at];
             obj_method "created_at"
               [%expr Sqlx.Ppx_runtime.expect_loaded created_at];
             obj_method "created_at_opt" [%expr created_at];
           ]
       | CUpdatedAt ->
           [
             obj_val "updated_at" [%expr [%e evar ~loc a].updated_at];
             obj_method "updated_at"
               [%expr Sqlx.Ppx_runtime.expect_loaded updated_at];
             obj_method "updated_at_opt" [%expr updated_at];
           ]
       | CNormal { c_ocaml_name = name; _ } ->
           let e_name = evar ~loc name in
           [
             obj_val name (pexp_field ~loc (evar ~loc a) (wloc (lident name)));
             obj_method name e_name;
             obj_method_set loc name;
             obj_method_with loc name;
           ]
  in
  let obj_vals_and_methods_for_derived =
    (schema.s_derived
    |> List.map (fun { d_name; d_opt; d_id_ident } ->
           let d_type =
             if d_opt then
               `Option (`User (`String, in_mod_ident d_id_ident "t"))
             else `User (`String, in_mod_ident d_id_ident "t")
           in
           let core_type = core_type_of_type loc d_type in
           (d_name, core_type)))
    @ (schema.s_user_defined
      |> List.map (fun { u_name; u_core_type; _ } ->
             (u_name, env_replace_mod_name env u_core_type)))
    @ (schema.s_related_field
      |> List.map (fun { r_name; r_relation; r_foregin_mod_name; _ } ->
             let type_M_t =
               ptyp_constr ~loc
                 { loc; txt = Ldot (Lident r_foregin_mod_name, "t") }
                 []
             in
             ( r_name,
               match r_relation with
               | `HasOne -> type_M_t
               | `HasOneOrZero ->
                   ptyp_constr ~loc { loc; txt = Lident "option" } [ type_M_t ]
               | `HasMany ->
                   ptyp_constr ~loc { loc; txt = Lident "list" } [ type_M_t ] ))
      )
    |> List.map @@ fun (name, typ) ->
       let typ = typ |> env_replace_mod_name env |> replace_rec_type schema in
       let e_name = evar ~loc name in
       [
         obj_val name [%expr (None : [%t typ] option)];
         obj_method name [%expr Sqlx.Ppx_runtime.expect_loaded [%e e_name]];
         obj_method_set loc name;
         obj_method_with loc name;
       ]
  in
  obj_vals_and_methods_for_columns @ obj_vals_and_methods_for_derived
  |> List.flatten

let expand_class_t env loc schemas =
  let replace_rec_type =
    let m =
      schemas
      |> List.map (fun schema ->
             ( Ldot (lident (env_get_mod env schema.s_ocaml_mod_name), "t"),
               lident ("t_" ^ schema.s_ocaml_mod_name) ))
      |> List.to_seq |> Hashtbl.of_seq
    in
    let rec aux schema ty =
      match ty.ptyp_desc with
      | Ptyp_constr ({ txt = ident; loc }, args) ->
          let txt =
            match Hashtbl.find_opt m ident with
            | Some x -> x
            | None ->
                if ident = Lident "t" then
                  Lident ("t_" ^ schema.s_ocaml_mod_name)
                else ident
          in
          {
            ty with
            ptyp_desc = Ptyp_constr ({ txt; loc }, List.map (aux schema) args);
          }
      | _ -> assert false
    in
    aux
  in
  schemas
  |> List.map (fun schema ->
         (* e.g., class ... and t_Account (a : Account.args) = object ... end and ... *)
         let a = gen_symbol ~prefix:"a" () in
         let name = { loc; txt = "t_" ^ schema.s_ocaml_mod_name } in
         let expr =
           pcl_fun ~loc Nolabel None
             (ppat_constraint ~loc
                (ppat_var ~loc { loc; txt = a })
                (ptyp_constr ~loc
                   {
                     loc;
                     txt =
                       Ldot
                         ( lident (env_get_mod env schema.s_ocaml_mod_name),
                           "args" );
                   }
                   []))
             (pcl_structure ~loc
                (class_structure ~self:(ppat_any ~loc)
                   ~fields:
                     (expand_class_t_fields env loc schema a replace_rec_type)))
         in
         class_infos ~loc ~virt:Concrete ~params:[] ~name ~expr)
  |> pstr_class ~loc

let expand_let_make loc schema =
  let xs =
    schema.s_columns
    |> List.map (fun c ->
           let name = ocaml_name_of_column c in
           (name, gen_symbol ~prefix:name ()))
    |> List.to_seq |> Hashtbl.of_seq
  in
  let body =
    schema.s_columns
    |> List.map (fun c ->
           let name = ocaml_name_of_column c in
           (wloc (lident name), evar ~loc (Hashtbl.find xs name)))
  in
  let body = [%expr fun () -> new t [%e pexp_record ~loc body None]] in
  let body =
    schema.s_columns
    |> List.fold_left
         (fun body c ->
           let name = ocaml_name_of_column c in
           pexp_fun ~loc
             (match type_of_column c with
             | `Option _ -> Optional name
             | _ -> Labelled name)
             None
             (ppat_var ~loc (wloc (Hashtbl.find xs name)))
             body)
         body
  in
  [%stri let make = [%e body]]

let expand_let_pack env loc schema =
  let x = gen_symbol () in
  let args =
    schema.s_columns
    |> List.map @@ fun c ->
       let decode =
         let decode_id l =
           Ldot (in_mod_ident (env_get_mod_ident env l) "ID", "of_int")
           |> wloc |> pexp_ident ~loc
         in
         let decode_user = function
           | `Int, Lident s ->
               env_open_mod_in_expr env loc schema.s_ocaml_mod_name
                 (pexp_ident ~loc (wloc (Lident (s ^ "_of_int"))))
           | `String, Lident s ->
               env_open_mod_in_expr env loc schema.s_ocaml_mod_name
                 (pexp_ident ~loc (wloc (Lident (s ^ "_of_string"))))
           | _ -> assert false
         in
         match c with
         | CID ->
             [%expr
               fun x -> x |> Sqlx.Value.expect_int |> ID.of_int |> Option.some]
         | CCreatedAt | CUpdatedAt ->
             [%expr fun x -> x |> Sqlx.Value.expect_timestamp |> Option.some]
         | CNormal { c_type; _ } -> (
             match c_type with
             | `Int -> [%expr fun x -> x |> Sqlx.Value.expect_int]
             | `String -> [%expr fun x -> x |> Sqlx.Value.expect_string]
             | `Bool -> [%expr fun x -> x |> Sqlx.Value.expect_bool]
             | `Ptime -> [%expr fun x -> x |> Sqlx.Value.expect_timestmap]
             | `ID l ->
                 [%expr fun x -> x |> Sqlx.Value.expect_int |> [%e decode_id l]]
             | `User (`Int, l) ->
                 [%expr
                   fun x ->
                     x |> Sqlx.Value.expect_int |> [%e decode_user (`Int, l)]]
             | `User (`String, l) ->
                 [%expr
                   fun x ->
                     x |> Sqlx.Value.expect_string
                     |> [%e decode_user (`String, l)]]
             | `Option `Int -> [%expr fun x -> x |> Sqlx.Value.expect_int_opt]
             | `Option `Bool -> [%expr fun x -> x |> Sqlx.Value.expect_bool_opt]
             | `Option `String ->
                 [%expr fun x -> x |> Sqlx.Value.expect_string_opt]
             | `Option `Ptime ->
                 [%expr fun x -> x |> Sqlx.Value.expect_timestamp_opt]
             | `Option (`ID l) ->
                 [%expr
                   fun x ->
                     x |> Sqlx.Value.expect_int_opt
                     |> Option.map [%e decode_id l]]
             | `Option (`User (`Int, l)) ->
                 [%expr
                   fun x ->
                     x |> Sqlx.Value.expect_int_opt
                     |> Option.map [%e decode_user (`Int, l)]]
             | `Option (`User (`String, l)) ->
                 [%expr
                   fun x ->
                     x |> Sqlx.Value.expect_string_opt
                     |> Option.map [%e decode_user (`String, l)]]
             | `Option (`Option _) -> assert false)
       in
       let ocaml_name = ocaml_name_of_column c in
       let sql_name = sql_name_of_column c in
       let typ = type_of_column c in
       let e =
         [%expr
           [%e evar ~loc x]
           |> List.assoc [%e estring ~loc sql_name]
           |> [%e decode]]
       in
       match typ with
       | `Option _ -> (Optional ocaml_name, e)
       | _ -> (Labelled ocaml_name, e)
  in
  let args = (Nolabel, [%expr ()]) :: args in
  let body = pexp_apply ~loc [%expr make] args in
  [%stri let pack [%p ppat_var ~loc (wloc x)] = [%e body]]

let expand_let_unpack env loc schema =
  let x = gen_symbol () in
  let body =
    schema.s_columns
    |> List.map @@ fun c ->
       match c with
       | CID ->
           [%expr
             "id",
               [%e evar ~loc x]#id_opt |> Option.map ID.to_int
               |> Sqlx.Value.of_int_opt]
       | CCreatedAt ->
           [%expr
             "created_at",
               [%e evar ~loc x]#created_at_opt |> Sqlx.Value.of_timestamp_opt]
       | CUpdatedAt ->
           [%expr
             "updated_at",
               [%e evar ~loc x]#updated_at_opt |> Sqlx.Value.of_timestamp_opt]
       | CNormal { c_type; _ } ->
           let encode =
             let encode_id l =
               Ldot (in_mod_ident (env_get_mod_ident env l) "ID", "to_int")
               |> wloc |> pexp_ident ~loc
             in
             let encode_user = function
               | `Int, Lident s ->
                   env_open_mod_in_expr env loc schema.s_ocaml_mod_name
                     (pexp_ident ~loc (wloc (Lident (s ^ "_to_int"))))
               | `String, Lident s ->
                   env_open_mod_in_expr env loc schema.s_ocaml_mod_name
                     (pexp_ident ~loc (wloc (Lident (s ^ "_to_string"))))
               | _ -> assert false
             in
             match c_type with
             | `Int -> [%expr fun x -> x |> Sqlx.Value.of_int]
             | `Bool -> [%expr fun x -> x |> Sqlx.Value.of_bool]
             | `String -> [%expr fun x -> x |> Sqlx.Value.of_string]
             | `Ptime -> [%expr fun x -> x |> Sqlx.Value.of_timestamp]
             | `ID l ->
                 [%expr fun x -> x |> [%e encode_id l] |> Sqlx.Value.of_int]
             | `User (`Int, l) ->
                 [%expr
                   fun x -> x |> [%e encode_user (`Int, l)] |> Sqlx.Value.of_int]
             | `User (`String, l) ->
                 [%expr
                   fun x ->
                     x |> [%e encode_user (`String, l)] |> Sqlx.Value.of_string]
             | `Option `Int -> [%expr fun x -> x |> Sqlx.Value.of_int_opt]
             | `Option `Bool -> [%expr fun x -> x |> Sqlx.Value.of_bool_opt]
             | `Option `String -> [%expr fun x -> x |> Sqlx.Value.of_string_opt]
             | `Option `Ptime ->
                 [%expr fun x -> x |> Sqlx.Value.of_timestamp_opt]
             | `Option (`ID l) ->
                 [%expr
                   fun x ->
                     x |> Option.map [%e encode_id l] |> Sqlx.Value.of_int_opt]
             | `Option (`User (`Int, l)) ->
                 [%expr
                   fun x ->
                     x
                     |> Option.map [%e encode_user (`Int, l)]
                     |> Sqlx.Value.of_int_opt]
             | `Option (`User (`String, l)) ->
                 [%expr
                   fun x ->
                     x
                     |> Option.map [%e encode_user (`String, l)]
                     |> Sqlx.Value.of_string_opt]
             | `Option (`Option _) -> assert false
           in
           pexp_tuple ~loc
             [
               estring ~loc (sql_name_of_column c);
               pexp_apply ~loc encode
                 [
                   ( Nolabel,
                     pexp_send ~loc (evar ~loc x)
                       (wloc (ocaml_name_of_column c)) );
                 ];
             ]
  in
  let body = elist ~loc body in
  [%stri
    let unpack ([%p ppat_var ~loc (wloc x)] : t) : (string * Sqlx.Value.t) list
        =
      [%e body]]

let expand_let_load_column loc schema col =
  let opt, name, mod_name =
    match col with
    | CNormal { c_ocaml_name; c_type = `ID l; _ } ->
        (false, c_ocaml_name, l.mod_ident)
    | CNormal { c_ocaml_name; c_type = `Option (`ID l); _ } ->
        (true, c_ocaml_name, l.mod_ident)
    | _ -> assert false
  in
  let mod_name =
    match mod_name with
    | None -> schema.s_ocaml_mod_name
    | Some (Lident s) -> s
    | _ -> assert false
  in
  let column_wo_id = column_name_wo_suffix_id name in
  let funname = "load_" ^ schema.s_ocaml_mod_name ^ "_" ^ column_wo_id in
  let select = lident ("select_" ^ mod_name) |> wloc |> pexp_ident ~loc in
  let x_set_column =
    (* e.g., x#set_account *)
    pexp_send ~loc [%expr x] (wloc ("set_" ^ column_wo_id))
  in
  let x_column_id =
    (* e.g., x#account_id *)
    pexp_send ~loc [%expr x] (wloc name)
  in
  value_binding ~loc
    ~pat:(ppat_var ~loc (wloc funname))
    ~expr:
      [%expr
        fun ?preload xs (c : Sqlx.Ppx_runtime.connection) ->
          let ids =
            xs
            |> [%e if opt then [%expr List.filter_map] else [%expr List.map]]
                 (fun x -> [%e x_column_id])
          in
          let ( >|= ) x f = Lwt.map f x in
          (match ids with
          | [] ->
              (* Prevent casting useless 'WHERE FALSE' queries *)
              Lwt.return []
          | _ -> [%e select] ?preload ~id:(`In ids) c)
          >|= Sqlx.Ppx_runtime.index_by (fun y -> y#id)
          >|= fun tbl ->
          xs
          |> List.iter @@ fun x ->
             [%e x_set_column]
               (Some
                  [%e
                    if opt then
                      [%expr
                        match [%e x_column_id] with
                        | None -> None
                        | Some y -> Some (Hashtbl.find tbl y)]
                    else [%expr Hashtbl.find tbl [%e x_column_id]]])]

let expand_let_load_user_defined_fields loc schema =
  schema.s_user_defined
  |> List.map (fun field ->
         let has_arg = Option.is_some field.u_preload_spec in
         let loader_name =
           "loader_" ^ schema.s_ocaml_mod_name ^ "_" ^ field.u_name
         in
         let load_name =
           "load_" ^ schema.s_ocaml_mod_name ^ "_" ^ field.u_name
         in
         if has_arg then
           [
             [%stri
               let [%p ppat_var ~loc (wloc loader_name)] =
                 ref (fun ?preload _ _ -> failwith "loader not found")];
             [%stri
               let [%p ppat_var ~loc (wloc load_name)] =
                fun ?preload xs c -> ![%e evar ~loc loader_name] ?preload xs c];
           ]
         else
           [
             [%stri
               let [%p ppat_var ~loc (wloc loader_name)] =
                 ref (fun _ _ -> failwith "loader not found")];
             [%stri
               let [%p ppat_var ~loc (wloc load_name)] =
                fun xs c -> ![%e evar ~loc loader_name] xs c];
           ])
  |> List.flatten

let expand_type_user_defined_field loc schema =
  let rtags =
    schema.s_user_defined
    |> List.map (fun f -> rtag ~loc { txt = f.u_name; loc } true [])
  in
  match rtags with
  | [] -> empty_variant_type ~loc "user_defined_field"
  | _ ->
      [%stri type user_defined_field = [%t ptyp_variant ~loc rtags Closed None]]

let expand_type_preload_spec env loc schemas =
  let rtags schema =
    (schema.s_derived
    |> List.map (fun f ->
           let name = f.d_name in
           let preload_spec_name =
             "preload_spec_"
             ^ Option.fold ~none:schema.s_ocaml_mod_name
                 ~some:(function Lident s -> s | _ -> assert false)
                 f.d_id_ident.mod_ident
           in
           let arg =
             ptyp_constr ~loc { loc; txt = Lident preload_spec_name } []
           in
           rtag ~loc { loc; txt = name } false [ arg ]))
    @ (schema.s_user_defined
      |> List.map (fun f ->
             match f.u_preload_spec with
             | None -> rtag ~loc (wloc f.u_name) true []
             | Some preload_spec_core_type ->
                 let rec replace_preload_spec_name ty =
                   (* Convert e.g., Status.preload_spec -> preload_spec_Status *)
                   match ty.ptyp_desc with
                   | Ptyp_constr (body, args) ->
                       let body =
                         match body with
                         | { txt = Ldot (Lident ident, "preload_spec"); loc } ->
                             let txt = Lident ("preload_spec_" ^ ident) in
                             { txt; loc }
                         | _ -> body
                       in
                       {
                         ty with
                         ptyp_desc =
                           Ptyp_constr
                             (body, List.map replace_preload_spec_name args);
                       }
                   | _ -> ty
                 in
                 rtag ~loc (wloc f.u_name) false
                   [
                     preload_spec_core_type |> env_replace_mod_name env
                     |> replace_preload_spec_name;
                   ]))
    @ (schema.s_related_field
      |> List.map (fun f ->
             let name = f.r_name in
             let preload_spec_name = "preload_spec_" ^ f.r_foregin_mod_name in
             let arg =
               ptyp_constr ~loc { loc; txt = Lident preload_spec_name } []
             in
             rtag ~loc (wloc name) false [ arg ]))
  in
  schemas
  |> List.map (fun schema ->
         let name = "preload_spec_" ^ schema.s_ocaml_mod_name in
         let name_elm = name ^ "_elm" in
         type_declaration ~loc ~name:{ loc; txt = name } ~params:[] ~cstrs:[]
           ~kind:Ptype_abstract ~private_:Public
           ~manifest:
             (Some
                (ptyp_constr ~loc
                   { loc; txt = Lident "list" }
                   [ ptyp_constr ~loc { loc; txt = Lident name_elm } [] ]))
         ::
         (match rtags schema with
         | [] ->
             [
               type_declaration ~loc ~name:{ loc; txt = name_elm } ~params:[]
                 ~cstrs:[] ~kind:(Ptype_variant []) ~private_:Public
                 ~manifest:None;
             ]
         | rtags ->
             [
               type_declaration ~loc ~params:[] ~cstrs:[] ~kind:Ptype_abstract
                 ~private_:Public ~name:{ loc; txt = name_elm }
                 ~manifest:(Some (ptyp_variant ~loc rtags Closed None));
             ]))
  |> List.flatten |> pstr_type ~loc Recursive

let expand_let_default_preload loc _schema =
  [%stri let default_preload : preload_spec ref = ref []]

let expand_where env loc schema where p =
  let mod_name = env_get_mod env schema.s_ocaml_mod_name in
  let column_pat =
    let txt = Ldot (Lident mod_name, "column") in
    ppat_type ~loc { loc; txt }
  in
  let string_of_column_expr =
    let txt = Ldot (Lident mod_name, "string_of_column") in
    pexp_ident ~loc { loc; txt }
  in
  schema.s_columns
  |> List.fold_left
       (fun body c ->
         match c with
         | CID | CCreatedAt | CUpdatedAt -> body
         | CNormal { c_ocaml_name; c_sql_name; c_type } ->
             let ident = pexp_ident ~loc (wloc (lident c_ocaml_name)) in
             let estr = estring ~loc c_sql_name in
             let where_id l =
               let mod_name =
                 (env_get_mod_ident env l).mod_ident
                 |> Option.value
                      ~default:
                        (Lident (env_get_mod env schema.s_ocaml_mod_name))
               in
               pexp_ident ~loc { loc; txt = Ldot (mod_name, "where_id") }
             in
             let where_id_opt l =
               let mod_name =
                 (env_get_mod_ident env l).mod_ident
                 |> Option.value
                      ~default:
                        (Lident (env_get_mod env schema.s_ocaml_mod_name))
               in
               pexp_ident ~loc { loc; txt = Ldot (mod_name, "where_id_opt") }
             in
             let encode_user = function
               | `Int, Lident s ->
                   env_open_mod_in_expr env loc schema.s_ocaml_mod_name
                     (pexp_ident ~loc (wloc (Lident (s ^ "_to_int"))))
               | `String, Lident s ->
                   env_open_mod_in_expr env loc schema.s_ocaml_mod_name
                     (pexp_ident ~loc (wloc (Lident (s ^ "_to_string"))))
               | _ -> assert false
             in
             let where =
               match c_type with
               | `Int -> [%expr Sqlx.Sql.where_int ~encode:Fun.id]
               | `Bool -> [%expr Sqlx.Sql.where_bool]
               | `String -> [%expr Sqlx.Sql.where_string ~encode:Fun.id]
               | `Ptime -> [%expr Sqlx.Sql.where_timestamp]
               | `ID l -> where_id l
               | `User (`Int, l) ->
                   [%expr Sqlx.Sql.where_int ~encode:[%e encode_user (`Int, l)]]
               | `User (`String, l) ->
                   [%expr
                     Sqlx.Sql.where_string ~encode:[%e encode_user (`String, l)]]
               | `Option `Int -> [%expr Sqlx.Sql.where_int_opt ~encode:Fun.id]
               | `Option `Bool -> [%expr Sqlx.Sql.where_bool_opt]
               | `Option `String ->
                   [%expr Sqlx.Sql.where_string_opt ~encode:Fun.id]
               | `Option `Ptime -> [%expr Sqlx.Sql.where_timestamp_opt]
               | `Option (`ID l) -> where_id_opt l
               | `Option (`User (`Int, l)) ->
                   [%expr
                     Sqlx.Sql.where_int_opt ~encode:[%e encode_user (`Int, l)]]
               | `Option (`User (`String, l)) ->
                   [%expr
                     Sqlx.Sql.where_string_opt
                       ~encode:[%e encode_user (`String, l)]]
               | _ -> assert false
             in
             [%expr [%e where] [%e estr] [%e ident] [%e body]])
       [%expr
         let sym_tbl = Hashtbl.create 0 in
         let parse x =
           let rec f = function
             | [%p column_pat] as c -> `C ([%e string_of_column_expr] c)
             | `Or (e1, e2) -> `Or (f e1, f e2)
             | `And (e1, e2) -> `And (f e1, f e2)
             | `Eq (e1, e2) -> `Eq (f e1, f e2)
             | `IsNull ([%p column_pat] as c) ->
                 `IsNull ([%e string_of_column_expr] c)
             | `IsNotNull ([%p column_pat] as c) ->
                 `IsNotNull ([%e string_of_column_expr] c)
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
         let name = ocaml_name_of_column c in
         pexp_fun ~loc (Optional name) None (ppat_var ~loc (wloc name)) body)
       body

let expand_let_select env loc schema =
  let mod_name = env_get_mod env schema.s_ocaml_mod_name in
  let select_expr =
    let txt = Ldot (Lident mod_name, "select") in
    pexp_ident ~loc { loc; txt }
  in
  let load_fields_expr =
    let txt = Lident ("load_" ^ schema.s_ocaml_mod_name ^ "_fields") in
    pexp_ident ~loc { loc; txt }
  in
  value_binding ~loc
    ~pat:(ppat_var ~loc (wloc ("select_" ^ schema.s_ocaml_mod_name)))
    ~expr:
      (expand_optionally_labelled_columns loc schema
         [%expr
           fun ?where ?(p = []) ?order_by ?limit ?preload c ->
             Lwt.bind
               ([%e select_expr] id created_at updated_at order_by limit c
                  [%e expand_where env loc schema [%expr where] [%expr p]])
               (fun xs ->
                 [%e load_fields_expr] ?preload xs c |> Lwt.map (fun () -> xs))])

let expand_let_load_fields env loc schema =
  let mod_name = env_get_mod env schema.s_ocaml_mod_name in
  let default_preload_expr =
    let txt = Ldot (Lident mod_name, "default_preload") in
    pexp_ident ~loc { loc; txt }
  in
  let cases =
    (schema.s_derived
    |> List.map @@ fun f ->
       case
         ~lhs:
           (ppat_variant ~loc f.d_name
              (Some (ppat_var ~loc (wloc "nested_preload"))))
         ~guard:None
         ~rhs:
           [%expr
             [%e
               pexp_ident ~loc
                 (wloc
                    (lident
                       ("load_" ^ schema.s_ocaml_mod_name ^ "_" ^ f.d_name)))]
               ~preload:nested_preload xs c])
    @ (schema.s_user_defined
      |> List.map @@ fun f ->
         let has_arg = Option.is_some f.u_preload_spec in
         let loader =
           pexp_ident ~loc
             (wloc
                (lident ("load_" ^ schema.s_ocaml_mod_name ^ "_" ^ f.u_name)))
         in
         case
           ~lhs:
             (ppat_variant ~loc f.u_name
                (if has_arg then Some (ppat_var ~loc (wloc "nested_preload"))
                 else None))
           ~guard:None
           ~rhs:
             (if has_arg then [%expr [%e loader] ~preload:nested_preload xs c]
              else [%expr [%e loader] xs c]))
    @ (schema.s_related_field
      |> List.map @@ fun f ->
         case
           ~lhs:
             (ppat_variant ~loc f.r_name
                (Some (ppat_var ~loc (wloc "nested_preload"))))
           ~guard:None
           ~rhs:
             [%expr
               [%e
                 pexp_ident ~loc
                   (wloc
                      (lident
                         ("load_" ^ schema.s_ocaml_mod_name ^ "_" ^ f.r_name)))]
                 ~preload:nested_preload xs c])
  in
  value_binding ~loc
    ~pat:(ppat_var ~loc (wloc ("load_" ^ schema.s_ocaml_mod_name ^ "_fields")))
    ~expr:
      (match cases with
      | [] -> [%expr fun ?preload:_ _ _ -> Lwt.return_unit]
      | _ ->
          [%expr
            fun ?preload xs c ->
              let preload =
                preload |> Option.value ~default:![%e default_preload_expr]
              in
              match xs with
              | [] ->
                  (* Prevent infinite loops. *)
                  (* NOTE: It will NOT prevent all infinite loops,
                     if there are any mutual recursive reference. *)
                  Lwt.return_unit
              | _ -> preload |> Lwt_list.iter_s [%e pexp_function ~loc cases]])

let expand_let_load_related_field loc schema rel =
  let mod_name = rel.r_foregin_mod_name in
  let select = lident ("select_" ^ mod_name) |> wloc |> pexp_ident ~loc in
  value_binding ~loc
    ~pat:
      (ppat_var ~loc
         { loc; txt = "load_" ^ schema.s_ocaml_mod_name ^ "_" ^ rel.r_name })
    ~expr:
      [%expr
        fun ?preload xs (c : Sqlx.Ppx_runtime.connection) ->
          let ( >|= ) x f = Lwt.map f x in
          let ids = xs |> List.map (fun x -> x#id) in
          [%e
            pexp_apply ~loc select
              [
                (Optional "preload", [%expr preload]);
                (Labelled rel.r_foregin_key, [%expr `In ids]);
                (Nolabel, [%expr c]);
              ]]
          >|= Sqlx.Ppx_runtime.index_by (fun y ->
                  [%e
                    let e =
                      pexp_send ~loc [%expr y] { loc; txt = rel.r_foregin_key }
                    in
                    if rel.r_is_foregin_key_opt then [%expr Option.get [%e e]]
                    else e])
          >|= fun tbl ->
          xs
          |> List.iter @@ fun x ->
             [%e pexp_send ~loc [%expr x] { loc; txt = "set_" ^ rel.r_name }]
               (Some
                  [%e
                    match rel.r_relation with
                    | `HasOne -> [%expr Hashtbl.find tbl x#id]
                    | `HasOneOrZero -> [%expr Hashtbl.find_opt tbl x#id]
                    | `HasMany -> [%expr Hashtbl.find_all tbl x#id]])]

let expand_let_select_and_load_columns_and_load_fields env loc schemas =
  schemas
  |> List.map (fun schema ->
         (schema.s_columns
         |> List.filter_map (fun c ->
                match c with
                | CNormal { c_type = `ID _ | `Option (`ID _); _ } ->
                    Some (expand_let_load_column loc schema c)
                | _ -> None))
         @ (schema.s_related_field
           |> List.map (expand_let_load_related_field loc schema))
         |> List.cons (expand_let_load_fields env loc schema)
         |> List.cons (expand_let_select env loc schema))
  |> List.flatten |> pstr_value ~loc Recursive

let expand_let_update loc _schema =
  [%stri
    let update ?preload xs c =
      Lwt.bind (update xs c) (fun xs ->
          load_fields ?preload xs c |> Lwt.map (fun () -> xs))]

let expand_let_insert loc _schema =
  [%stri
    let insert ?preload xs c =
      Lwt.bind (insert xs c) (fun xs ->
          load_fields ?preload xs c |> Lwt.map (fun () -> xs))]

let expand_let_count env loc schema =
  [%stri
    let count =
      [%e
        expand_optionally_labelled_columns loc schema
          [%expr
            fun ?where ?(p = []) c ->
              count id created_at updated_at c
                [%e expand_where env loc schema [%expr where] [%expr p]]]]]

let expand_let_all loc _schema = [%stri let all c = select c]

let expand_let_get_many loc schema =
  [%stri
    let get_many =
      [%e
        expand_optionally_labelled_columns loc schema
          [%expr
            fun ?where ?(p = []) ?order_by ?limit ?preload c ->
              [%e
                pexp_apply ~loc [%expr select]
                  (schema.s_columns
                  |> List.map @@ fun c ->
                     let name = ocaml_name_of_column c in
                     ( Optional name,
                       [%expr
                         [%e pexp_ident ~loc (wloc (lident name))]
                         |> Option.map @@ fun x ->
                            [%e
                              match c with
                              | CNormal { c_type = `Option _; _ } ->
                                  [%expr
                                    Option.fold ~none:`EqNone
                                      ~some:(fun x -> `Eq x)
                                      x]
                              | _ -> [%expr `Eq x]]] ))]
                ?where ?order_by ?limit ~p ?preload c]]]

let expand_let_get_one loc schema =
  [%stri
    let get_one =
      [%e
        expand_optionally_labelled_columns loc schema
          [%expr
            fun ?where ?(p = []) ?order_by ?preload c ->
              [%e
                pexp_apply ~loc [%expr get_many]
                  (schema.s_columns
                  |> List.map @@ fun c ->
                     let name = ocaml_name_of_column c in
                     (Optional name, pexp_ident ~loc (wloc (lident name))))]
                ~limit:1 ?where ?order_by ~p ?preload c
              |> Lwt.map Sqlx.Ppx_runtime.expect_single_row]]]

let include_struct_module_ID loc =
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
    end]

let include_module_lident loc name =
  name |> lident |> wloc |> pmod_ident ~loc |> include_infos ~loc
  |> pstr_include ~loc

let generate_modules env loc schemas f =
  schemas
  |> List.map @@ fun schema ->
     let name = schema.s_ocaml_mod_name in
     let old_name = env_get_mod_opt env name |> Option.value ~default:"" in
     let new_name = gen_symbol ~prefix:name () in
     let generated_module =
       f old_name new_name schema |> pmod_structure ~loc |> fun expr ->
       module_binding ~loc ~name:{ loc; txt = Some new_name } ~expr
       |> pstr_module ~loc
     in
     env_add_mod env name new_name;
     generated_module

let expand_0 env loc schemas =
  generate_modules env loc schemas @@ fun _old_name _name schema ->
  schema.s_custom_header
  @ [
      include_struct_module_ID loc;
      [%stri let table_name = [%e estring ~loc schema.s_sql_name]];
      expand_type_column loc schema;
      expand_let_columns loc schema;
      expand_let_string_of_column loc schema;
      expand_type_user_defined_field loc schema;
    ]

let expand_0' env loc schemas = [ expand_type_preload_spec env loc schemas ]

let expand_1 env loc schemas =
  generate_modules env loc schemas @@ fun old_name _name schema ->
  [
    include_module_lident loc old_name;
    [%stri
      type preload_spec =
        [%t
          ptyp_constr ~loc
            { loc; txt = Lident ("preload_spec_" ^ schema.s_ocaml_mod_name) }
            []]];
    expand_let_default_preload loc schema;
    expand_type_args env loc schema;
  ]

let expand_2 env loc schemas = [ expand_class_t env loc schemas ]

let expand_3 env loc schemas =
  generate_modules env loc schemas @@ fun old_name _name schema ->
  [
    include_module_lident loc old_name;
    pstr_class ~loc
      [
        class_infos ~loc ~virt:Concrete ~params:[] ~name:{ loc; txt = "t" }
          ~expr:
            (pcl_constr ~loc
               { loc; txt = lident ("t_" ^ schema.s_ocaml_mod_name) }
               []);
      ];
    expand_let_make loc schema;
    expand_let_pack env loc schema;
    expand_let_unpack env loc schema;
    [%stri
      let after_create_commit_callbacks :
          (t -> Sqlx.Ppx_runtime.connection -> unit Lwt.t) list ref =
        ref []];
    [%stri
      let after_destroy_commit_callbacks :
          (t -> Sqlx.Ppx_runtime.connection -> unit Lwt.t) list ref =
        ref []];
    [%stri
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
        let id x = x#id
        let after_create_commit_callbacks = after_create_commit_callbacks
        let after_destroy_commit_callbacks = after_destroy_commit_callbacks
      end)];
  ]

let expand_4 env loc schemas =
  (schemas |> List.map (expand_let_load_user_defined_fields loc) |> List.flatten)
  @ [ expand_let_select_and_load_columns_and_load_fields env loc schemas ]

let expand_5 env loc schemas =
  generate_modules env loc schemas @@ fun old_name _new_name schema ->
  let exp_ident l = pexp_ident ~loc { loc; txt = lident l } in
  [
    include_module_lident loc old_name;
    [%stri let select = [%e exp_ident ("select_" ^ schema.s_ocaml_mod_name)]];
    [%stri
      let load_fields =
        [%e exp_ident ("load_" ^ schema.s_ocaml_mod_name ^ "_fields")]];
    expand_let_update loc schema;
    expand_let_insert loc schema;
    expand_let_count env loc schema;
    expand_let_all loc schema;
    expand_let_get_many loc schema;
    expand_let_get_one loc schema;
    [%stri
      let save_one ?preload (x : t) c =
        match x#id_opt with
        | None -> insert ?preload [ x ] c |> Lwt.map List.hd
        | Some _ -> update ?preload [ x ] c |> Lwt.map List.hd];
  ]
  @ (schema.s_user_defined
    |> List.map (fun field ->
           let old_loader_name =
             "loader_" ^ schema.s_ocaml_mod_name ^ "_" ^ field.u_name
           in
           let old_load_name =
             "load_" ^ schema.s_ocaml_mod_name ^ "_" ^ field.u_name
           in
           let new_loader_name = "loader_" ^ field.u_name in
           let new_load_name = "load_" ^ field.u_name in
           [
             [%stri
               let [%p ppat_var ~loc { loc; txt = new_loader_name }] =
                 [%e pexp_ident ~loc { loc; txt = Lident old_loader_name }]];
             [%stri
               let [%p ppat_var ~loc { loc; txt = new_load_name }] =
                 [%e pexp_ident ~loc { loc; txt = Lident old_load_name }]];
           ])
    |> List.flatten)

let expand_6 env loc schemas =
  schemas
  |> List.map @@ fun schema ->
     let gen_mod_name = env_get_mod env schema.s_ocaml_mod_name in
     pmod_ident ~loc { loc; txt = Lident gen_mod_name } |> fun expr ->
     module_binding ~loc ~name:{ loc; txt = Some schema.s_ocaml_mod_name } ~expr
     |> pstr_module ~loc

let wrap_include loc xs =
  xs |> pmod_structure ~loc |> include_infos ~loc |> pstr_include ~loc

let expand ~ctxt (xs : module_binding list) =
  let loc = !Ast_helper.default_loc in
  let schemas =
    xs
    |> List.map (fun (x : module_binding) ->
           let mod_name = Option.get x.pmb_name.txt in
           Ast_pattern.(parse (pmod_structure __)) loc x.pmb_expr @@ fun ys ->
           let schema = construct_schema ctxt ys in
           { schema with s_ocaml_mod_name = mod_name })
    |> analyze_schemas
  in
  let env = init_expand_env in
  [
    expand_0;
    expand_0';
    expand_1;
    expand_2;
    expand_3;
    expand_4;
    expand_5;
    expand_6;
  ]
  |> List.fold_left
       (fun acc f -> wrap_include loc (f env loc schemas) :: acc)
       []
  |> List.rev |> wrap_include loc

let sqlx_schemas =
  Extension.V3.declare "sqlx.schemas" Extension.Context.structure_item
    Ast_pattern.(pstr (pstr_recmodule __ ^:: nil))
    expand

let () =
  Driver.register_transformation
    ~rules:[ Ppxlib.Context_free.Rule.extension sqlx_schemas ]
    "sqlx.schemas";
  ()
