type expr =
  [ `True
  | `False
  | `C of string (* column *)
  | `M of string (* marker *)
  | `UM of string (* marker made by user *)
  | `Eq of expr * expr
  | `And of expr * expr
  | `Or of expr * expr
  | `InInts of expr * int list
  | `IsNull of string
  | `IsNotNull of string
  | `Raw of string ]

let expr_to_string ?const_f (e : expr) : string =
  let prec_const = (1000, 1000) in
  let prec = function
    | `True | `False | `C _ | `M _ | `UM _ | `Raw _ -> prec_const
    | `IsNull _ | `IsNotNull _ -> (100, 100)
    | `InInts _ -> (90, 90)
    | `Eq _ -> (79, 80)
    | `And _ -> (70, 69)
    | `Or _ -> (60, 59)
  in
  let paren ~p1 ~p2 ~p ~left ~right ~mid =
    (if snd p1 < fst p then "(" ^ left ^ ")" else left)
    ^ " " ^ mid ^ " "
    ^ if fst p2 < snd p then "(" ^ right ^ ")" else right
  in
  let string_of_op = function
    | `Eq _ -> "="
    | `And _ -> "AND"
    | `Or _ -> "OR"
    | _ -> assert false
  in
  let rec f = function
    | `True -> "TRUE"
    | `False -> "FALSE"
    | `C s -> s
    | `M s -> const_f |> Option.fold ~none:("~" ^ s) ~some:(fun f -> f (`M s))
    | `UM s -> const_f |> Option.fold ~none:(":" ^ s) ~some:(fun f -> f (`UM s))
    | (`Eq (e1, e2) | `And (e1, e2) | `Or (e1, e2)) as op ->
        paren ~p:(prec op) ~p1:(prec e1) ~p2:(prec e2) ~left:(f e1)
          ~right:(f e2) ~mid:(string_of_op op)
    | `InInts (_, []) -> f `False
    | `InInts (e, vals) as v ->
        let right =
          "(" ^ (vals |> List.map string_of_int |> String.concat ", ") ^ ")"
        in
        paren ~p:(prec v) ~p1:(prec e) ~p2:prec_const ~left:(f e) ~mid:"IN"
          ~right
    | `IsNull c -> c ^ " IS NULL"
    | `IsNotNull c -> c ^ " IS NOT NULL"
    | `Raw s -> s
  in
  f e

let exprs_to_strings_with_numbered_markers exprs param =
  let param_h = Hashtbl.create 0 in
  let const_f k =
    match Hashtbl.find_opt param_h k with
    | Some (i, _v) -> "$" ^ string_of_int i
    | None ->
        let v = List.assoc k param in
        let i = Hashtbl.length param_h + 1 in
        Hashtbl.add param_h k (i, v);
        "$" ^ string_of_int i
  in
  let exprs = exprs |> List.map (fun e -> expr_to_string ~const_f e) in
  let param =
    param_h |> Hashtbl.to_seq_values |> List.of_seq |> List.sort compare
    |> List.map snd
  in
  (exprs, param)

let where_nullable name ptn (where, param) =
  match ptn with
  | `EqNone ->
      let where = `IsNull name :: where in
      (where, param)
  | `NeqNone ->
      let where = `IsNotNull name :: where in
      (where, param)

let where_int name ptn ((where, param) as cond) =
  let rec f = function
    | `Eq (x : int) ->
        let where = `Eq (`C name, `M name) :: where in
        let param = (`M name, `Int x) :: param in
        (where, param)
    | `In [ v ] -> f (`Eq v)
    | `In vs ->
        let where = `InInts (`C name, vs) :: where in
        (where, param)
  in
  match ptn with None -> cond | Some ptn -> f ptn

let where_int_opt name ptn cond =
  let f = function
    | `Eq _ | `In _ -> where_int name ptn cond
    | (`EqNone | `NeqNone) as ptn -> where_nullable name ptn cond
  in
  match ptn with None -> cond | Some ptn -> f ptn

let where_string ~encode name ptn ((where, param) as cond) =
  match ptn with
  | None -> cond
  | Some (`Eq s) ->
      let where = `Eq (`C name, `M name) :: where in
      let param = (`M name, `String (encode s)) :: param in
      (where, param)

let where_string_opt ~encode name ptn cond =
  match ptn with
  | None -> cond
  | Some ((`EqNone | `NeqNone) as ptn) -> where_nullable name ptn cond
  | Some (`Eq _ as ptn) -> where_string ~encode name (Some ptn) cond

let where_timestamp name ptn ((where, param) as cond) =
  match ptn with
  | None -> cond
  | Some (`Eq t) ->
      let where = `Eq (`C name, `M name) :: where in
      let param = (`M name, `Timestamp t) :: param in
      (where, param)

type param = [ `M of string | `UM of string ] * Value.t

let and_exprs =
  List.fold_left
    (fun acc x -> match acc with `True -> x | y -> `And (y, x))
    `True

let select ~columns ~table_name ~order_by ~limit
    ((where : expr list), (param : param list)) =
  let where = and_exprs where in
  let columns = match columns with `Star -> "*" | `Count_star -> "COUNT(*)" in
  let table = table_name in
  let where, param = exprs_to_strings_with_numbered_markers [ where ] param in
  let where = " WHERE " ^ List.hd where in
  let order_by =
    match order_by with
    | None -> ""
    | Some l ->
        " ORDER BY "
        ^ (l
          |> List.map (function c, `ASC -> c | c, `DESC -> c ^ " DESC")
          |> String.concat ", ")
  in
  let limit =
    match limit with None -> "" | Some i -> " LIMIT " ^ string_of_int i
  in
  let sql = "SELECT " ^ columns ^ " FROM " ^ table ^ where ^ order_by ^ limit in
  (sql, param)

let update ~table_name ~columns ~unpacked
    ((where : expr list), (param : param list)) =
  let where = and_exprs (`Eq (`C "id", `M "id") :: where) in
  let param =
    (unpacked |> List.map (fun (column, value) -> (`M column, value))) @ param
  in
  let set =
    columns
    |> List.filter_map (function
         | "id" | "created_at" -> None
         | "updated_at" -> Some ("updated_at", `Raw "now()")
         | c -> Some (c, `M c))
  in
  let table, returning = (table_name, Some "*") in
  let exprs, param =
    exprs_to_strings_with_numbered_markers (where :: List.map snd set) param
  in
  let where = List.hd exprs in
  let set =
    List.tl exprs |> List.combine set
    |> List.map (fun ((column, _), e) -> column ^ " = " ^ e)
    |> String.concat ", "
  in
  let returning =
    match returning with None -> "" | Some s -> " RETURNING " ^ s
  in
  let sql = "UPDATE " ^ table ^ " SET " ^ set ^ " WHERE " ^ where ^ returning in
  (sql, param)

let insert ~table_name ~columns ~unpacked =
  let param =
    unpacked |> List.map (fun (column, value) -> (`M column, value))
  in
  let columns, values =
    columns
    |> List.filter_map (function
         | "id" -> None
         | ("created_at" | "updated_at") as c -> Some (c, `Raw "now()")
         | c -> Some (c, `M c))
    |> List.split
  in
  let table, returning = (table_name, Some "*") in
  let exprs, param = exprs_to_strings_with_numbered_markers values param in
  let values = "(" ^ String.concat ", " exprs ^ ")" in
  let returning =
    match returning with None -> "" | Some s -> " RETURNING " ^ s
  in
  let columns = "(" ^ String.concat "," columns ^ ")" in
  let sql =
    "INSERT INTO " ^ table ^ " " ^ columns ^ " VALUES " ^ values ^ returning
  in
  (sql, param)

let delete ~table_name ~id =
  let table, where, param =
    (table_name, `Eq (`C "id", `M "id"), [ (`M "id", `Int id) ])
  in
  let exprs, param = exprs_to_strings_with_numbered_markers [ where ] param in
  let where = List.hd exprs in
  let sql = "DELETE FROM " ^ table ^ " WHERE " ^ where in
  (sql, param)
