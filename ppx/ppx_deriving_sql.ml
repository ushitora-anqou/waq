open Ppxlib
open Ast_builder.Default

module Attrs = struct
  let table_name =
    Attribute.declare "sql.table_name" Attribute.Context.type_declaration
      Ast_pattern.(pstr (pstr_eval (estring __) nil ^:: nil))
      Fun.id

  let auto_increment : (label_declaration, unit) Attribute.t =
    Attribute.declare "sql.auto_increment" Attribute.Context.label_declaration
      Ast_pattern.(pstr nil)
      ()
end

let core_type_mapper = function
  | [%type: int] -> `Int
  | [%type: int option] -> `IntOption
  | [%type: string] -> `String
  | [%type: string option] -> `StringOption
  | [%type: Ptime.t] -> `Ptime
  | [%type: Ptime.t option] -> `PtimeOption
  | _ -> assert false

let accessor_impl ptype_name (ld : label_declaration) =
  let loc = ld.pld_loc in
  Ast_helper.with_default_loc loc @@ fun () ->
  [%stri
    let [%p ppat_var ~loc { loc; txt = ptype_name ^ "_" ^ ld.pld_name.txt }] =
     fun x ->
      [%e pexp_field ~loc [%expr x] { loc; txt = lident ld.pld_name.txt }]]

let pack_impl loc (fields : label_declaration list) =
  Ast_helper.with_default_loc loc @@ fun () ->
  let funname = "pack" in
  let record_fields : (Longident.t Location.loc * expression) list =
    fields
    |> List.map (fun f ->
           let fname = f.pld_name.txt in
           let e_fname = estring ~loc fname in
           ( (* label *) { loc; txt = lident fname },
             (* field *)
             match core_type_mapper f.pld_type with
             | `Int -> [%expr List.assoc [%e e_fname] l |> Sql.Value.expect_int]
             | `IntOption ->
                 [%expr List.assoc [%e e_fname] l |> Sql.Value.expect_int_opt]
             | `String ->
                 [%expr List.assoc [%e e_fname] l |> Sql.Value.expect_string]
             | `StringOption ->
                 [%expr
                   List.assoc [%e e_fname] l |> Sql.Value.expect_string_opt]
             | `Ptime ->
                 [%expr List.assoc [%e e_fname] l |> Sql.Value.expect_timestamp]
             | `PtimeOption ->
                 [%expr
                   List.assoc [%e e_fname] l |> Sql.Value.expect_timestamp_opt]
           ))
  in
  [%stri
    let [%p ppat_var ~loc { loc; txt = funname }] =
     fun (l : Sql.single_query_result) ->
      [%e pexp_record ~loc record_fields None]]

let wrap_value_with_type loc pld_type value =
  match core_type_mapper pld_type with
  | `Int -> [%expr `Int [%e value]]
  | `IntOption ->
      [%expr match [%e value] with None -> `Null | Some v -> `Int v]
  | `String -> [%expr `String [%e value]]
  | `StringOption ->
      [%expr match [%e value] with None -> `Null | Some v -> `String v]
  | `Ptime -> [%expr `Timestamp [%e value]]
  | `PtimeOption ->
      [%expr match [%e value] with None -> `Null | Some v -> `Timestamp v]

let unpack_impl loc (fields : label_declaration list) =
  Ast_helper.with_default_loc loc @@ fun () ->
  let funname = "unpack" in
  let body =
    fields
    |> List.map (fun f ->
           let field_name = f.pld_name.txt (* e.g., id *) in
           let key = estring ~loc field_name (* e.g., "id" *) in
           let value =
             (* e.g., x.id *)
             pexp_field ~loc [%expr x] { loc; txt = lident field_name }
           in
           (* e.g., `Int x.id *)
           let value = wrap_value_with_type loc f.pld_type value in
           (* e.g., ("id", `Int x.id) *)
           [%expr [%e key], [%e value]])
    |> elist ~loc
  in
  [%stri let [%p ppat_var ~loc { loc; txt = funname }] = fun x -> [%e body]]

let query_impl loc =
  Ast_helper.with_default_loc loc @@ fun () ->
  [%stri
    let query ~p sql = do_query @@ fun c -> List.map pack =|< Sql.query c sql ~p]

let query_row_impl loc =
  Ast_helper.with_default_loc loc @@ fun () ->
  [%stri
    let query_row ~p sql = do_query @@ fun c -> pack =|< Sql.query_row c sql ~p]

let named_query_impl loc =
  Ast_helper.with_default_loc loc @@ fun () ->
  [%stri
    let named_query sql x =
      do_query @@ fun c -> List.map pack =|< Sql.named_query c sql ~p:(unpack x)]

let named_query_row_impl loc =
  Ast_helper.with_default_loc loc @@ fun () ->
  [%stri
    let named_query_row sql x =
      do_query @@ fun c -> pack =|< Sql.named_query_row c sql ~p:(unpack x)]

let save_one_impl loc (fields : label_declaration list) (td : type_declaration)
    =
  Attribute.get Attrs.table_name td
  |> Option.map @@ fun table_name ->
     Ast_helper.with_default_loc loc @@ fun () ->
     let funname = "save_one" in
     let columns =
       fields
       |> List.filter_map (fun f ->
              match Attribute.get Attrs.auto_increment f with
              | None -> Some f.pld_name.txt
              | Some _ -> None)
     in
     let sql =
       "INSERT INTO " ^ table_name ^ "("
       ^ (columns |> String.concat ", ")
       ^ ") VALUES ("
       ^ (columns |> List.map (( ^ ) ":") |> String.concat ", ")
       ^ ") RETURNING *"
     in
     [%stri
       let [%p ppat_var ~loc { loc; txt = funname }] =
        fun x -> named_query_row [%e estring ~loc sql] x]

let general_where_impl kind loc (fields : label_declaration list)
    (td : type_declaration) =
  Attribute.get Attrs.table_name td
  |> Option.map @@ fun table_name ->
     Ast_helper.with_default_loc loc @@ fun () ->
     let funname =
       match kind with
       | `GetOne -> "get_one"
       | `GetMany -> "get_many"
       | `Delete -> "delete"
     in
     let execute_func_ast, sql_prefix =
       match kind with
       | `GetOne ->
           ( [%expr Lwt.map pack (Sql.named_query_row c sql ~p:params)],
             "SELECT * FROM " ^ table_name ^ " WHERE " )
       | `GetMany ->
           ( [%expr Lwt.map (List.map pack) (Sql.named_query c sql ~p:params)],
             "SELECT * FROM " ^ table_name ^ " WHERE " )
       | `Delete ->
           ( [%expr Sql.named_execute c sql ~p:params],
             "DELETE FROM " ^ table_name ^ " WHERE " )
     in

     let columns = fields |> List.map (fun f -> f.pld_name.txt) in
     let varnames =
       "where" :: "p" :: columns
       |> List.map (fun name -> (name, gen_symbol ~prefix:name ()))
     in

     let parts =
       fields
       |> List.map @@ fun f ->
          let name = f.pld_name.txt in
          let varname = varnames |> List.assoc name in
          [%expr
            (* e.g.,
               Option.fold ~none:(query, params) ~some:(fun x ->
                 ("id = :id" :: query, ("id", `Int x) :: params))
                 id
            *)
            Option.fold ~none:(query, params)
              ~some:(fun x ->
                ( [%e
                    match core_type_mapper f.pld_type with
                    | `Int | `String | `Ptime ->
                        estring ~loc (name ^ " = :" ^ name)
                    | `IntOption | `StringOption | `PtimeOption ->
                        estring ~loc (name ^ " IS NOT DISTINCT FROM :" ^ name)]
                  :: query,
                  ( [%e estring ~loc name],
                    [%e wrap_value_with_type loc f.pld_type [%expr x]] )
                  :: params ))
              [%e pexp_ident ~loc { loc; txt = Lident varname }]]
     in

     let body =
       (* e.g.,
          fun ?id -> fun ?email -> fun ?where ?p () ->
            let query, params = ([], Option.value ~default:[] p) in
            let query, params = {{ 0th of parts }} in
            ...
            let query, params = {{ nth of parts }} in
            let query = where |> Option.fold ~none:query ~some:(fun x -> x :: query) in
            let sql =
              "SELECT * FROM {{ table_name }} WHERE"
              ^ (query |> List.map (fun x -> "(" ^ x ^ ")") |> String.concat " AND ")
            in
            do_query @@ fun c -> Lwt.map pack (Sql.named_query_row c sql ~p:params)
       *)
       let var_p =
         pexp_ident ~loc { loc; txt = Lident (varnames |> List.assoc "p") }
       in
       let var_where =
         pexp_ident ~loc { loc; txt = Lident (varnames |> List.assoc "where") }
       in
       varnames
       |> List.fold_left
            (fun body (name, varname) ->
              pexp_fun ~loc (Optional name) None
                (ppat_var ~loc { loc; txt = varname })
                body)
            [%expr
              fun () ->
                let query, params = ([], Option.value ~default:[] [%e var_p]) in
                [%e
                  parts
                  |> List.fold_left
                       (fun a e ->
                         [%expr
                           let query, params = [%e e] in
                           [%e a]])
                       [%expr
                         let query =
                           [%e var_where]
                           |> Option.fold ~none:query ~some:(fun x ->
                                  x :: query)
                         in
                         let sql =
                           [%e estring ~loc sql_prefix]
                           ^ (query
                             |> List.map (fun x -> "(" ^ x ^ ")")
                             |> String.concat " AND ")
                         in
                         do_query @@ fun c -> [%e execute_func_ast]]]]
     in
     [%stri let [%p ppat_var ~loc { loc; txt = funname }] = [%e body]]

let generate_impl ~ctxt (_rec_flag, type_declarations) =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  type_declarations
  |> List.map (fun (td : type_declaration) ->
         match td with
         | {
          ptype_kind = Ptype_abstract | Ptype_variant _ | Ptype_open;
          ptype_loc;
          _;
         } ->
             let ext =
               Location.error_extensionf ~loc:ptype_loc
                 "Cannot derive accessors for non record types"
             in
             [ Ast_builder.Default.pstr_extension ~loc ext [] ]
         | { ptype_loc; ptype_kind = Ptype_record fields; _ } ->
             [
               pack_impl ptype_loc fields;
               unpack_impl ptype_loc fields;
               query_impl ptype_loc;
               query_row_impl ptype_loc;
               named_query_impl ptype_loc;
               named_query_row_impl ptype_loc;
             ]
             @ ([
                  save_one_impl ptype_loc fields td;
                  general_where_impl `GetOne ptype_loc fields td;
                  general_where_impl `GetMany ptype_loc fields td;
                  general_where_impl `Delete ptype_loc fields td;
                ]
               |> List.filter_map Fun.id))
  |> List.concat

let impl_generator =
  Deriving.Generator.V2.make_noarg generate_impl
    ~attributes:[ Attribute.T Attrs.table_name ]

let my_deriver = Deriving.add "sql" ~str_type_decl:impl_generator
