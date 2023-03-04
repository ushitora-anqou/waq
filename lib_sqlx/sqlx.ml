open Lwt.Infix

let failwithf f = Printf.ksprintf failwith f
let expect_loaded = function None -> failwith "not preloaded" | Some x -> x
let ( |.> ) f g x = f x |> g
let ignore_lwt p = Lwt.map (fun _ -> ()) p

let iota n =
  let rec f acc = function 0 -> acc | n -> f ((n - 1) :: acc) (n - 1) in
  f [] n

let index_by f l =
  let h = Hashtbl.create (List.length l) in
  l |> List.iter (fun x -> Hashtbl.replace h (f x) x);
  h

module Value = struct
  type t =
    [ `Null
    | `String of string
    | `Int of int
    | `Float of float
    | `Timestamp of Ptime.t ]
  [@@deriving show]

  type null_t =
    [ t | `NullString of string option | `NullTimestamp of Ptime.t option ]

  let normalize : null_t -> t = function
    | `NullString None -> `Null
    | `NullString (Some s) -> `String s
    | `NullTimestamp None -> `Null
    | `NullTimestamp (Some t) -> `Timestamp t
    | #t as v -> v

  let expect_int : t -> int = function
    | `Int i -> i
    | v -> failwithf "Expect int, got: %s" (show v)

  let expect_int_opt : t -> int option = function
    | `Null -> None
    | `Int i -> Some i
    | v -> failwithf "Expect int or null, got: %s" (show v)

  let expect_string : t -> string = function
    | `String s -> s
    | v -> failwithf "Expect string, got: %s" (show v)

  let expect_string_opt : t -> string option = function
    | `Null -> None
    | `String s -> Some s
    | v -> failwithf "Expect string or null, got: %s" (show v)

  let expect_timestamp : t -> Ptime.t = function
    | `Timestamp t -> t
    | v -> failwithf "Expect timestamp, got: %s" (show v)

  let expect_timestamp_opt : t -> Ptime.t option = function
    | `Null -> None
    | `Timestamp t -> Some t
    | v -> failwithf "Expect timestamp or null, got: %s" (show v)

  let of_int (n : int) = `Int n
  let of_string (s : string) = `String s
  let of_timestamp (t : Ptime.t) = `Timestamp t

  let of_string_opt (s : string option) =
    match s with None -> `Null | Some s -> `String s
end

module Internal = struct
  module Pg = Postgresql

  exception NoRowFound

  type value = Value.t
  type params = value list
  type single_query_result = (string * value) list
  type query_result = single_query_result list

  module type Driver = sig
    type connection
    type statement

    val validate : connection -> bool Lwt.t
    val check : connection -> (bool -> unit) -> unit
    val connect : string -> connection Lwt.t
    val disconnect : connection -> unit Lwt.t
    val prepare : connection -> string -> statement Lwt.t
    val execute_stmt : connection -> statement -> params -> unit Lwt.t
    val query_stmt : connection -> statement -> params -> query_result Lwt.t
  end

  module Make (D : Driver) = struct
    type connection = D.connection
    type connection_pool = connection Lwt_pool.t

    let connect_pool n uri =
      let validate = D.validate in
      let check = D.check in
      let dispose = D.disconnect in
      Lwt_pool.create n ~validate ~check ~dispose (fun () -> D.connect uri)

    let use : connection_pool -> (connection -> 'a Lwt.t) -> 'a Lwt.t =
      Lwt_pool.use

    let log =
      let open String in
      let max_length = 30 in
      let conv = function
        | `Null -> "NULL"
        | `String s ->
            "\""
            ^ ((if length s > max_length then sub s 0 max_length ^ "..." else s)
              |> escaped)
            ^ "\""
        | `Int i -> string_of_int i
        | `Float f -> string_of_float f
        | `Timestamp t -> Ptime.to_rfc3339 t
      in
      fun sql params ->
        Logq.debug (fun m ->
            m "\o033[1;34m%s\o033[0m [%s]"
              (sql |> split_on_char '\n' |> List.map trim
              |> List.filter (( <> ) "")
              |> List.filter (starts_with ~prefix:"--" |.> not)
              |> concat " " |> trim)
              (params |> List.map conv |> concat "; "))

    let execute ?(p = []) (c : connection) (sql : string) : unit Lwt.t =
      let p = p |> List.map Value.normalize in
      log sql p;
      let%lwt stmt = D.prepare c sql in
      D.execute_stmt c stmt p

    let query ?(p = []) c sql =
      let p = p |> List.map Value.normalize in
      log sql p;
      let%lwt stmt = D.prepare c sql in
      D.query_stmt c stmt p

    let query_row ?(p = []) (c : connection) (sql : string) :
        single_query_result Lwt.t =
      let%lwt res = query c sql ~p in
      match res with row :: _ -> Lwt.return row | [] -> raise NoRowFound

    let resolve_named_sql =
      let re = Re.(Pcre.re {|:([_a-zA-Z][_a-zA-Z0-9]*)|} |> compile) in
      fun p sql ->
        let names = ref [] in
        let sql =
          Re.replace ~all:true re
            ~f:(fun g ->
              let name = Re.Group.get g 1 in
              names := name :: !names;
              "$" ^ string_of_int (List.length !names))
            sql
        in
        let p = !names |> List.rev |> List.map (fun k -> List.assoc k p) in
        (p, sql)

    let named_execute ?(p = []) (c : connection) (sql : string) : unit Lwt.t =
      let p, sql = resolve_named_sql p sql in
      execute ~p c sql

    let named_query ?(p = []) (c : connection) (sql : string) :
        query_result Lwt.t =
      let p, sql = resolve_named_sql p sql in
      query ~p c sql

    let named_query_row ?(p = []) (c : connection) (sql : string) :
        single_query_result Lwt.t =
      let p, sql = resolve_named_sql p sql in
      query_row ~p c sql
  end

  module PgDriver = struct
    type connection = { c : Pg.connection } [@@deriving make]

    type statement = {
      name : string;
      params : Pg.ftype list;
      fields : (string * Pg.ftype) list;
    }
    [@@deriving make]

    let ftype_mapper (ty : Pg.ftype) f =
      match ty with
      | TEXT | VARCHAR -> f `String
      | INT2 | INT4 | INT8 -> f `Int
      | FLOAT4 | FLOAT8 -> f `Float
      | TIMESTAMP | TIMESTAMPTZ -> f `Timestamp
      | _ ->
          failwithf "Not implemented ftype in ftype_mapper: %s"
            (Pg.string_of_ftype ty)

    let value_to_string_for_pg ~(ty : Pg.ftype) (v : value) : string =
      ftype_mapper ty @@ fun ty' ->
      match (ty', v) with
      | _, `Null -> Pg.null
      | `String, `String s -> s
      | `Int, `Int i -> string_of_int i
      | `Float, `Float f -> string_of_float f
      | `Timestamp, `Timestamp t -> Ptime.to_rfc3339 ~frac_s:6 t
      | _ ->
          failwithf "Invalid pair of type and value: \"%s\" and \"%s\""
            (Pg.string_of_ftype ty) (Value.show v)

    let value_of_string_for_pg ~(ty : Pg.ftype) (s : string option) : value =
      let aux s = function
        | `String -> `String s
        | `Int -> `Int (int_of_string s)
        | `Float -> `Float (float_of_string s)
        | `Timestamp -> (
            (* e.g., 2023-01-13 14:02:17.4242
                     012345678901234567890123 *)
            match Ptime.of_rfc3339 (s ^ "Z") with
            | Ok (t, _, _) -> `Timestamp t
            | Error _ -> failwithf "Invalid format of timestamp: %s" s)
        | _ ->
            failwithf "Not implemented pg datatype: %s" (Pg.string_of_ftype ty)
      in
      s |> Option.fold ~none:`Null ~some:(fun s -> ftype_mapper ty (aux s))

    let raise_error msg = function
      | `PgError (e : Pg.error) ->
          failwithf "Pg %s failed: %s" msg (Pg.string_of_error e)
      | `Result (r : Pg.result) ->
          failwithf "Pg %s failed (%s): %s" msg
            (Pg.Error_code.to_string r#error_code)
            r#error

    let rec finish_conn socket_fd connect_poll = function
      | Pg.Polling_failed ->
          (*Logq.debug (fun m -> m "Polling failed");*)
          Lwt.return_unit
      | Polling_ok ->
          (*Logq.debug (fun m -> m "Polling ok");*)
          Lwt.return_unit
      | Polling_reading ->
          (*Logq.debug (fun m -> m "Polling reading");*)
          ignore_lwt @@ Lwt.choose [ Lwt_unix.wait_read socket_fd ];%lwt
          finish_conn socket_fd connect_poll (connect_poll ())
      | Polling_writing ->
          (*Logq.debug (fun m -> m "Polling writing");*)
          ignore_lwt @@ Lwt.choose [ Lwt_unix.wait_write socket_fd ];%lwt
          finish_conn socket_fd connect_poll (connect_poll ())

    let wait_for_result (c : Pg.connection) =
      c#consume_input;
      while%lwt c#is_busy do
        let socket_fd = c#socket |> Obj.magic |> Lwt_unix.of_unix_file_descr in
        ignore_lwt @@ Lwt.choose [ Lwt_unix.wait_read socket_fd ];%lwt
        Lwt.return c#consume_input
      done

    let fetch_result (c : connection) =
      try%lwt
        wait_for_result c.c;%lwt
        Lwt.return c.c#get_result
      with Pg.Error e -> raise_error __FUNCTION__ @@ `PgError e

    let fetch_single_result (c : connection) =
      match%lwt fetch_result c with
      | None -> assert false
      | Some r ->
          let%lwt r' = fetch_result c in
          assert (r' = None);
          Lwt.return r

    let send_query (c : connection) (sql : string) : unit =
      try c.c#send_query sql
      with Pg.Error e -> raise_error __FUNCTION__ @@ `PgError e

    let send_prepare =
      let index = ref 0 in
      fun (c : connection) (sql : string) : string ->
        let name = "prepared_statement_" ^ string_of_int !index in
        index := !index + 1;
        try
          c.c#send_prepare name sql;
          name
        with Pg.Error e -> raise_error __FUNCTION__ @@ `PgError e

    let send_query_prepared (c : connection) (stmt : statement)
        (params : value list) =
      let params =
        List.combine stmt.params params
        |> List.map (fun (ty, v) -> value_to_string_for_pg ~ty v)
        |> Array.of_list
      in
      try c.c#send_query_prepared ~params stmt.name
      with Pg.Error e -> raise_error __FUNCTION__ @@ `PgError e

    let send_describe_prepared (c : connection) (name : string) =
      try c.c#send_describe_prepared name
      with Pg.Error e -> raise_error __FUNCTION__ @@ `PgError e

    let expect_command_ok (name : string) (r : Pg.result Lwt.t) :
        Pg.result Lwt.t =
      let%lwt r = r in
      match r#status with
      | Command_ok -> Lwt.return r
      | _ -> raise_error name @@ `Result r

    let execute_direct (c : connection) (sql : string) : unit Lwt.t =
      send_query c sql;
      ignore_lwt @@ expect_command_ok __FUNCTION__ @@ fetch_single_result c

    let prepare (c : connection) (sql : string) : statement Lwt.t =
      (* Prepare *)
      let name = send_prepare c sql in
      ignore_lwt @@ expect_command_ok __FUNCTION__ @@ fetch_single_result c;%lwt

      (* Describe *)
      send_describe_prepared c name;
      let%lwt desc = expect_command_ok __FUNCTION__ @@ fetch_single_result c in
      let params = desc#nparams |> iota |> List.map desc#paramtype in
      let fields =
        desc#nfields |> iota |> List.map (fun i -> (desc#fname i, desc#ftype i))
      in

      let stmt = make_statement ~name ~params ~fields () in
      Lwt.return stmt

    let execute_stmt (c : connection) (stmt : statement) (params : value list) :
        unit Lwt.t =
      send_query_prepared c stmt params;
      ignore_lwt @@ expect_command_ok __FUNCTION__ @@ fetch_single_result c

    let query_stmt (c : connection) (stmt : statement) (params : value list) :
        query_result Lwt.t =
      send_query_prepared c stmt params;
      let%lwt r = fetch_single_result c in
      if r#status <> Tuples_ok then raise_error __FUNCTION__ @@ `Result r;
      assert (List.length stmt.fields = r#nfields);

      r#ntuples |> iota
      |> List.map (fun row ->
             stmt.fields
             |> List.mapi (fun col (name, ty) ->
                    let s =
                      if r#getisnull row col then None
                      else Some (r#getvalue row col)
                    in
                    (name, value_of_string_for_pg ~ty s)))
      |> Lwt.return

    let connect (uri : string) =
      let u = Uri.of_string uri in
      let host = Uri.host u |> Option.value ~default:"127.0.0.1" in
      let port = Uri.port u |> Option.value ~default:5432 |> string_of_int in
      let user = Uri.user u |> Option.value ~default:(Unix.getlogin ()) in
      let password = Uri.password u |> Option.value ~default:"" in
      let dbname =
        let s = Uri.path u in
        String.sub s 1 (String.length s - 1)
      in
      match
        new Pg.connection ~host ~port ~dbname ~user ~password ~startonly:true ()
      with
      | exception Pg.Error e -> raise_error __FUNCTION__ @@ `PgError e
      | c ->
          finish_conn
            (c#socket |> Obj.magic |> Lwt_unix.of_unix_file_descr)
            (fun () -> c#connect_poll)
            Polling_writing;%lwt
          if c#status = Bad then
            failwithf "Pg connection failed (2): %s" c#error_message;
          assert (c#status = Ok);
          c#set_nonblocking true;
          let c = make_connection ~c in
          execute_direct c "SET TimeZone TO 'UTC'";%lwt
          Lwt.return c

    let disconnect (c : connection) : unit Lwt.t =
      (try c.c#finish
       with Pg.Error e -> raise_error __FUNCTION__ @@ `PgError e);
      Lwt.return_unit

    let validate (c : connection) : bool Lwt.t =
      c.c#consume_input;
      match c.c#status with
      | (exception Pg.Error _) | Pg.Ok -> Lwt.return_false
      | _ -> Lwt.return_true

    let check (c : connection) f =
      f
      @@
      match c.c#status with
      | exception Pg.Error _ -> false
      | Pg.Ok -> true
      | _ -> false
  end

  include Make (PgDriver)
end

class type connection =
  object ('a)
    method query : string -> Value.t list -> (string * Value.t) list list Lwt.t
    method query_row : string -> Value.t list -> (string * Value.t) list Lwt.t
    method execute : string -> Value.t list -> unit Lwt.t
    method enqueue_task_after_commit : ('a -> unit Lwt.t) -> unit Lwt.t
    (*method enqueued_tasks_after_commit : ('a -> unit Lwt.t) list*)
  end

module Sql = struct
  type expr =
    [ `True
    | `C of string (* column *)
    | `M of string (* marker *)
    | `UM of string (* marker made by user *)
    | `Eq of expr * expr
    | `And of expr * expr
    | `InInts of expr * int list
    | `IsNull of string
    | `IsNotNull of string
    | `Raw of string ]

  let expr_to_string ?const_f (e : expr) : string =
    let prec_const = (1000, 1000) in
    let prec = function
      | `True | `C _ | `M _ | `UM _ | `Raw _ -> prec_const
      | `IsNull _ | `IsNotNull _ -> (100, 100)
      | `InInts _ -> (90, 90)
      | `Eq _ -> (79, 80)
      | `And _ -> (70, 69)
    in
    let paren ~p1 ~p2 ~p ~left ~right ~mid =
      (if snd p1 < fst p then "(" ^ left ^ ")" else left)
      ^ " " ^ mid ^ " "
      ^ if fst p2 < snd p then "(" ^ right ^ ")" else right
    in
    let string_of_op = function
      | `Eq _ -> "="
      | `And _ -> "AND"
      | _ -> assert false
    in
    let rec f = function
      | `True -> "TRUE"
      | `C s -> s
      | `M s -> const_f |> Option.fold ~none:("~" ^ s) ~some:(fun f -> f (`M s))
      | `UM s ->
          const_f |> Option.fold ~none:(":" ^ s) ~some:(fun f -> f (`UM s))
      | (`Eq (e1, e2) | `And (e1, e2)) as op ->
          paren ~p:(prec op) ~p1:(prec e1) ~p2:(prec e2) ~left:(f e1)
            ~right:(f e2) ~mid:(string_of_op op)
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

  let where_int_opt name ptn ((where, param) as cond) =
    let f = function
      | `Eq _ | `In _ -> where_int name ptn cond
      | `EqNone ->
          let where = `IsNull name :: where in
          (where, param)
      | `NeqNone ->
          let where = `IsNotNull name :: where in
          (where, param)
    in
    match ptn with None -> cond | Some ptn -> f ptn

  let where_string ?encode:_ _name ptn cond =
    match ptn with
    | None -> cond
    | Some `NotImplemented -> failwith "where_string: not implemented"

  let where_string_opt ?encode:_ _name ptn cond =
    match ptn with
    | None -> cond
    | Some `NotImplemented -> failwith "where_string: not implemented"

  let where_timestamp _name ptn cond =
    match ptn with
    | None -> cond
    | Some `NotImplemented -> failwith "where_timestamp: not implemented"

  type param = [ `M of string | `UM of string ] * Value.t

  let and_exprs =
    List.fold_left
      (fun acc x -> match acc with `True -> x | y -> `And (y, x))
      `True

  let select ~table_name ~order_by ~limit
      ((where : expr list), (param : param list)) =
    let where = and_exprs where in
    let columns, table = ("*", table_name) in
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
    let sql =
      "SELECT " ^ columns ^ " FROM " ^ table ^ where ^ order_by ^ limit
    in
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
    let sql =
      "UPDATE " ^ table ^ " SET " ^ set ^ " WHERE " ^ where ^ returning
    in
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
end

module Make (M : sig
  module ID : sig
    type t

    val to_int : t -> int
  end

  type column
  type t

  val columns : column list
  val string_of_column : column -> string
  val table_name : string
  val unpack : t -> (string * Value.t) list
  val pack : (string * Value.t) list -> t
  val id : t -> ID.t
  val after_create_commit_callbacks : (t -> connection -> unit Lwt.t) list ref
end) =
struct
  let where_id name ptn cond =
    match ptn with
    | None -> cond
    | Some ptn -> (
        match ptn with
        | `Eq (x : M.ID.t) ->
            Sql.where_int name (Some (`Eq (M.ID.to_int x))) cond
        | `In (vals : M.ID.t list) ->
            Sql.where_int name (Some (`In (vals |> List.map M.ID.to_int))) cond)

  let select id created_at updated_at order_by limit preload (c : connection)
      preload_spec cond =
    let sql, param =
      Sql.select ~table_name:M.table_name
        ~order_by:
          (order_by
          |> Option.map (List.map (fun (k, ad) -> (M.string_of_column k, ad))))
        ~limit
      @@ where_id "id" id
      @@ Sql.where_timestamp "created_at" created_at
      @@ Sql.where_timestamp "updated_at" updated_at
      @@ cond
    in
    let%lwt rows = c#query sql param >|= List.map M.pack in
    preload_spec
    |> Lwt_list.iter_s (fun (column, f) ->
           if List.mem column preload then f rows c else Lwt.return_unit);%lwt
    Lwt.return rows

  let update (xs : M.t list) (c : connection) =
    xs
    |> Lwt_list.map_s @@ fun x ->
       let sql, param =
         Sql.update ~table_name:M.table_name
           ~columns:M.(columns |> List.map string_of_column)
           ~unpacked:(M.unpack x)
         @@ ([], [])
       in
       c#query_row sql param >|= M.pack

  let insert (xs : M.t list) (c : connection) =
    (* FIXME: Efficient impl *)
    let%lwt rows =
      xs
      |> Lwt_list.map_s (fun x ->
             let sql, param =
               Sql.insert ~table_name:M.table_name
                 ~columns:M.(List.map string_of_column columns)
                 ~unpacked:(M.unpack x)
             in
             c#query_row sql param >|= M.pack)
    in
    rows
    |> Lwt_list.iter_s (fun row ->
           !M.after_create_commit_callbacks
           |> Lwt_list.iter_s (fun f -> c#enqueue_task_after_commit (f row)));%lwt
    Lwt.return rows

  let delete (xs : M.t list) (c : connection) =
    (* FIXME: Efficient impl *)
    xs
    |> Lwt_list.iter_p @@ fun x ->
       let sql, param =
         Sql.delete ~table_name:M.table_name ~id:(x |> M.id |> M.ID.to_int)
       in
       c#execute sql param
end

module Account = struct
  module Internal = struct
    module ID : sig
      type t

      val of_int : int -> t
      val to_int : t -> int
    end = struct
      type t = int

      let of_int = Fun.id
      let to_int = Fun.id
    end

    type column =
      [ `id | `created_at | `updated_at | `username | `domain | `display_name ]

    let columns : column list =
      [ `id; `created_at; `updated_at; `username; `domain; `display_name ]

    let string_of_column = function
      | `id -> "id"
      | `created_at -> "created_at"
      | `updated_at -> "updated_at"
      | `username -> "username"
      | `domain -> "domain"
      | `display_name -> "display_name"

    let table_name = "accounts"

    class t ?id ?created_at ?updated_at ~username ?domain ~display_name () =
      object
        val mutable id = id
        method id : ID.t = expect_loaded id
        method id_opt : ID.t option = id
        val mutable created_at = created_at
        method created_at : Ptime.t = expect_loaded created_at
        method created_at_opt : Ptime.t option = created_at
        val mutable updated_at = updated_at
        method updated_at : Ptime.t = expect_loaded updated_at
        method updated_at_opt : Ptime.t option = updated_at
        val username : string = username
        method username : string = username
        method with_username (username : string) = {<username>}
        val domain : string option = domain
        method domain : string option = domain
        method with_domain (domain : string option) = {<domain>}
        method display_name : string = display_name
      end

    let pack (x : (string * Value.t) list) : t =
      new t
        ~id:(List.assoc "id" x |> Value.expect_int |> ID.of_int)
        ~created_at:(List.assoc "created_at" x |> Value.expect_timestamp)
        ~updated_at:(List.assoc "updated_at" x |> Value.expect_timestamp)
        ~username:(List.assoc "username" x |> Value.expect_string)
        ?domain:(List.assoc "domain" x |> Value.expect_string_opt)
        ~display_name:(List.assoc "display_name" x |> Value.expect_string)
        ()

    let unpack (x : t) : (string * Value.t) list =
      let cons x xs = match x with None -> xs | Some x -> x :: xs in
      cons
        (x#id_opt
        |> Option.map (fun x -> ("id", x |> ID.to_int |> Value.of_int)))
      @@ cons
           (x#created_at_opt
           |> Option.map (fun x -> ("created_at", Value.of_timestamp x)))
      @@ cons
           (x#updated_at_opt
           |> Option.map (fun x -> ("updated_at", Value.of_timestamp x)))
      @@ [
           ("username", x#username |> Value.of_string);
           ("domain", x#domain |> Value.of_string_opt);
           ("display_name", x#display_name |> Value.of_string);
         ]

    let id x = x#id
    let created_at x = x#created_at
    let updated_at x = x#updated_at

    let after_create_commit_callbacks : (t -> connection -> unit Lwt.t) list ref
        =
      ref []
  end

  include Internal
  include Make (Internal)

  let select ?id ?order_by ?limit ?created_at ?updated_at ?username ?domain
      ?display_name ?(preload = []) (c : connection) =
    select id created_at updated_at order_by limit preload c []
    @@ Sql.where_string "username" username
    @@ Sql.where_string_opt "domain" domain
    @@ Sql.where_string "display_name" display_name
    @@ ([], [])

  let is_local a = Option.is_none a#domain
  let is_remote a = Option.is_some a#domain

  let _ =
    Internal.after_create_commit_callbacks :=
      [
        (fun r c ->
          let%lwt [ r' ] = select ~id:(`Eq r#id) c [@@warning "-8"] in
          Printf.printf ">>>>>>>>>> %d %s\n" (ID.to_int r'#id) r'#username;
          Lwt.return_unit);
      ]
end

module Status = struct
  type t = int
end

module Notification = struct
  (* v User defined functions *)
  type activity_type_t = [ `Status | `Favourite | `Follow ]

  let string_of_activity_type_t : activity_type_t -> string = function
    | `Status -> "Status"
    | `Favourite -> "Favourite"
    | `Follow -> "Follow"

  let activity_type_t_of_string : string -> activity_type_t = function
    | "Status" -> `Status
    | "Favourite" -> `Favourite
    | "Follow" -> `Follow
    | _ -> failwith "activity_type_t_of_string: invalid input"

  type typ_t = [ `reblog | `favourite | `follow ]

  let string_of_typ_t : typ_t -> string = function
    | `reblog -> "reblog"
    | `favourite -> "favourite"
    | `follow -> "follow"

  let typ_t_of_string : string -> typ_t = function
    | "reblog" -> `reblog
    | "favourite" -> `favourite
    | "follow" -> `follow
    | _ -> failwith "type_t_of_string: invalid input"

  (* ^ User defined functions *)

  module Internal = struct
    module ID : sig
      type t

      val of_int : int -> t
      val to_int : t -> int
    end = struct
      type t = int

      let of_int = Fun.id
      let to_int = Fun.id
    end

    type id = ID.t

    let id_of_int = ID.of_int
    let int_of_id = ID.to_int

    type column =
      [ `id
      | `created_at
      | `updated_at
      | `activity_id
      | `activity_type
      | `account_id
      | `from_account_id
      | `typ ]

    let columns : column list =
      [
        `id;
        `created_at;
        `updated_at;
        `activity_id;
        `activity_type;
        `account_id;
        `from_account_id;
        `typ;
      ]

    let string_of_column : column -> string = function
      | `id -> "id"
      | `created_at -> "created_at"
      | `updated_at -> "updated_at"
      | `activity_id -> "activity_id"
      | `activity_type -> "activity_type"
      | `account_id -> "account_id"
      | `from_account_id -> "from_account_id"
      | `typ -> "type"

    let table_name = "notifications"

    class t ?id ?created_at ?updated_at ~activity_id ~activity_type ~account_id
      ~from_account_id ?typ () =
      object
        val mutable id = id
        method id : id = expect_loaded id
        method id_opt : id option = id
        val mutable created_at = created_at
        method created_at : Ptime.t = expect_loaded created_at
        method created_at_opt : Ptime.t option = created_at
        val mutable updated_at = updated_at
        method updated_at : Ptime.t = expect_loaded updated_at
        method updated_at_opt : Ptime.t option = updated_at
        method account_id : Account.ID.t = account_id
        method from_account_id : Account.ID.t = from_account_id
        method activity_id : int = activity_id
        method activity_type : activity_type_t = activity_type
        method typ : typ_t option = typ
        val mutable account = None
        method account : Account.t = expect_loaded account
        method set_account (x : Account.t) = account <- Some x
        val mutable from_account = None
        method from_account : Account.t = expect_loaded from_account
        method set_from_account (x : Account.t) = from_account <- Some x
        val mutable target_status = None
        method target_status : Status.t = expect_loaded target_status
        method set_target_status (x : Status.t) = target_status <- Some x
      end

    let pack (x : (string * Value.t) list) : t =
      new t
        ~id:(List.assoc "id" x |> Value.expect_int |> id_of_int)
        ~created_at:(List.assoc "created_at" x |> Value.expect_timestamp)
        ~updated_at:(List.assoc "updated_at" x |> Value.expect_timestamp)
        ~activity_id:(List.assoc "activity_id" x |> Value.expect_int)
        ~activity_type:
          (List.assoc "activity_type" x
          |> Value.expect_string |> activity_type_t_of_string)
        ?typ:
          (List.assoc (string_of_column `typ) x
          |> Value.expect_string_opt |> Option.map typ_t_of_string)
        ~account_id:
          (List.assoc "account_id" x |> Value.expect_int |> Account.ID.of_int)
        ~from_account_id:
          (List.assoc "from_account_id" x
          |> Value.expect_int |> Account.ID.of_int)
        ()

    let unpack (x : t) : (string * Value.t) list =
      let cons x xs = match x with None -> xs | Some x -> x :: xs in
      cons
        (x#id_opt
        |> Option.map (fun x -> ("id", x |> int_of_id |> Value.of_int)))
      @@ cons
           (x#created_at_opt
           |> Option.map (fun x -> ("created_at", Value.of_timestamp x)))
      @@ cons
           (x#updated_at_opt
           |> Option.map (fun x -> ("updated_at", Value.of_timestamp x)))
      @@ [
           ("activity_id", x#activity_id |> Value.of_int);
           ( "activity_type",
             x#activity_type |> string_of_activity_type_t |> Value.of_string );
           ( string_of_column `typ,
             x#typ |> Option.map string_of_typ_t |> Value.of_string_opt );
           ("account_id", x#account_id |> Account.ID.to_int |> Value.of_int);
           ( "from_account_id",
             x#from_account_id |> Account.ID.to_int |> Value.of_int );
         ]

    let id x = x#id
    let created_at x = x#created_at
    let updated_at x = x#updated_at

    let after_create_commit_callbacks : (t -> connection -> unit Lwt.t) list ref
        =
      ref []
  end

  include Internal
  include Make (Internal)

  let load_account (xs : t list) (c : connection) =
    let ids = xs |> List.map (fun x -> x#account_id) in
    Account.select ~id:(`In ids) c >|= index_by (fun y -> y#id) >|= fun tbl ->
    xs |> List.iter (fun x -> Hashtbl.find tbl x#account_id |> x#set_account)

  let select ?id ?created_at ?updated_at ?account_id ?from_account_id
      ?activity_id ?activity_type ?typ ?order_by ?limit
      ?(preload : [ `account ] list = []) c =
    select id created_at updated_at order_by limit preload c
      [ (`account, load_account) ]
    @@ Account.where_id "account_id" account_id
    @@ Account.where_id "from_account_id" from_account_id
    @@ Sql.where_int "activity_id" activity_id
    @@ Sql.where_string ~encode:string_of_activity_type_t "activity_type"
         activity_type
    @@ Sql.where_string_opt ~encode:string_of_typ_t (string_of_column `typ) typ
    @@ ([], [])
end

module Db = struct
  let global_pool = ref None

  let initialize url =
    let pool = Internal.connect_pool 10 url in
    global_pool := Some pool

  let do_query f = Internal.use (Option.get !global_pool) f

  let maybe_no_row e =
    match%lwt e with
    | exception Internal.NoRowFound -> Lwt.return_none
    | res -> Lwt.return_some res

  class connection c =
    object (self : 'a)
      method query (sql : string) (param : Value.t list)
          : (string * Value.t) list list Lwt.t =
        Internal.query c sql ~p:(param : Value.t list :> Value.null_t list)

      method query_row (sql : string) (param : Value.t list)
          : (string * Value.t) list Lwt.t =
        Internal.query_row c sql ~p:(param : Value.t list :> Value.null_t list)

      method execute (sql : string) (param : Value.t list) : unit Lwt.t =
        Internal.execute c sql ~p:(param : Value.t list :> Value.null_t list)

      method enqueue_task_after_commit (f : 'a -> unit Lwt.t) : unit Lwt.t =
        (* NOTE: Currently, sqlx does not support transactions, so all tasks
           should be executed immediately after enqueued *)
        f self
    end

  let e q = do_query @@ fun c -> q (new connection c)

  let debug_drop_all_tables_in_db () =
    do_query @@ fun c ->
    Internal.execute c
      {|
-- Thanks to: https://stackoverflow.com/a/36023359
DO $$ DECLARE
    r RECORD;
BEGIN
    -- if the schema you operate on is not "current", you will want to
    -- replace current_schema() in query with 'schematodeletetablesfrom'
    -- *and* update the generate 'DROP...' accordingly.
    FOR r IN (SELECT tablename FROM pg_tables WHERE schemaname = current_schema()) LOOP
        EXECUTE 'DROP TABLE IF EXISTS ' || quote_ident(r.tablename) || ' CASCADE';
    END LOOP;
END $$|}
end
