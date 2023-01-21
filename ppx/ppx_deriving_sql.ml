open Ppxlib
open Ast_builder.Default

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
             match f.pld_type with
             | [%type: int] ->
                 [%expr List.assoc [%e e_fname] l |> Sql.Value.expect_int]
             | [%type: int option] ->
                 [%expr List.assoc [%e e_fname] l |> Sql.Value.expect_int_opt]
             | [%type: string] ->
                 [%expr List.assoc [%e e_fname] l |> Sql.Value.expect_string]
             | [%type: string option] ->
                 [%expr
                   List.assoc [%e e_fname] l |> Sql.Value.expect_string_opt]
             | [%type: Ptime.t] ->
                 [%expr List.assoc [%e e_fname] l |> Sql.Value.expect_timestamp]
             | [%type: Ptime.t option] ->
                 [%expr
                   List.assoc [%e e_fname] l |> Sql.Value.expect_timestamp_opt]
             | _ -> assert false ))
  in
  [%stri
    let [%p ppat_var ~loc { loc; txt = funname }] =
     fun (l : Sql.single_query_result) ->
      [%e pexp_record ~loc record_fields None]]

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
           let value =
             (* e.g., `Int x.id *)
             match f.pld_type with
             | [%type: int] -> [%expr `Int [%e value]]
             | [%type: int option] ->
                 [%expr
                   match [%e value] with None -> `Null | Some v -> `Int v]
             | [%type: string] -> [%expr `String [%e value]]
             | [%type: string option] ->
                 [%expr
                   match [%e value] with None -> `Null | Some v -> `String v]
             | [%type: Ptime.t] -> [%expr `Timestamp [%e value]]
             | [%type: Ptime.t option] ->
                 [%expr
                   match [%e value] with
                   | None -> `Null
                   | Some v -> `Timestamp v]
             | _ -> assert false
           in
           (* e.g., ("id", `Int x.id) *)
           [%expr [%e key], [%e value]])
    |> elist ~loc
  in
  [%stri let [%p ppat_var ~loc { loc; txt = funname }] = fun x -> [%e body]]

let query_impl loc =
  Ast_helper.with_default_loc loc @@ fun () ->
  [%stri let query ~p c sql = List.map pack =|< Sql.query c sql ~p]

let query_row_impl loc =
  Ast_helper.with_default_loc loc @@ fun () ->
  [%stri let query_row ~p c sql = pack =|< Sql.query_row c sql ~p]

let named_query_impl loc =
  Ast_helper.with_default_loc loc @@ fun () ->
  [%stri
    let named_query c sql x =
      List.map pack =|< Sql.named_query c sql ~p:(unpack x)]

let named_query_row_impl loc =
  Ast_helper.with_default_loc loc @@ fun () ->
  [%stri
    let named_query_row c sql x =
      pack =|< Sql.named_query_row c sql ~p:(unpack x)]

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
             ])
  |> List.concat

let impl_generator = Deriving.Generator.V2.make_noarg generate_impl
let my_deriver = Deriving.add "sql" ~str_type_decl:impl_generator
