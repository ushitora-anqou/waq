open Lwt.Infix [@@warning "-33"]
open Sqlx
open Util

[@@@warning "-32"]

type connection = Ppx_runtime.connection

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
        method id : ID.t = Ppx_runtime.expect_loaded id
        method id_opt : ID.t option = id
        val mutable created_at = created_at
        method created_at : Ptime.t = Ppx_runtime.expect_loaded created_at
        method created_at_opt : Ptime.t option = created_at
        val mutable updated_at = updated_at
        method updated_at : Ptime.t = Ppx_runtime.expect_loaded updated_at
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
  include Ppx_runtime.Make (Internal)

  let select ?id ?order_by ?limit ?created_at ?updated_at ?username ?domain
      ?display_name ?(preload = []) (c : connection) =
    select id created_at updated_at order_by limit preload c []
    @@ Sql.where_string "username" username
    @@ Sql.where_string_opt "domain" domain
    @@ Sql.where_string "display_name" display_name
    @@ ([], [])

  let is_local a = Option.is_none a#domain
  let is_remote a = Option.is_some a#domain

  (*
  let _ =
    Internal.after_create_commit_callbacks :=
      [
        (fun r c ->
          let%lwt [ r' ] = select ~id:(`Eq r#id) c [@@warning "-8"] in
          Printf.printf ">>>>>>>>>> %d %s\n" (ID.to_int r'#id) r'#username;
          Lwt.return_unit);
      ]
      *)
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
        method id : id = Ppx_runtime.expect_loaded id
        method id_opt : id option = id
        val mutable created_at = created_at
        method created_at : Ptime.t = Ppx_runtime.expect_loaded created_at
        method created_at_opt : Ptime.t option = created_at
        val mutable updated_at = updated_at
        method updated_at : Ptime.t = Ppx_runtime.expect_loaded updated_at
        method updated_at_opt : Ptime.t option = updated_at
        method account_id : Account.ID.t = account_id
        method from_account_id : Account.ID.t = from_account_id
        method activity_id : int = activity_id
        method activity_type : activity_type_t = activity_type
        method typ : typ_t option = typ
        val mutable account = None
        method account : Account.t = Ppx_runtime.expect_loaded account
        method set_account (x : Account.t) = account <- Some x
        val mutable from_account = None
        method from_account : Account.t = Ppx_runtime.expect_loaded from_account
        method set_from_account (x : Account.t) = from_account <- Some x
        val mutable target_status = None

        method target_status : Status.t =
          Ppx_runtime.expect_loaded target_status

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
  include Ppx_runtime.Make (Internal)

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
  include Engine.Make (Engine.PgDriver)

  let debug_drop_all_tables_in_db () =
    e @@ fun c ->
    c#execute
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
      []
end

let print_sql_param sql param =
  Printf.printf ">>> %s\n" sql;
  Printf.printf ">>> %s\n" (param |> List.map Value.show |> String.concat ", ");
  ()

let foo _ _ =
  let sql, param =
    Sql.select ~table_name:"notifications"
      ~order_by:(Some [ ("id", `ASC); ("account_id", `DESC) ])
      ~limit:(Some 1)
      ( [ `Eq (`C "id", `M "foo"); `InInts (`C "account_id", [ 2; 3; 4 ]) ],
        [ (`M "foo", Value.of_int 42) ] )
  in
  print_sql_param sql param;

  let sql, param =
    Sql.update ~table_name:"notifications" ~columns:[ "account_id" ]
      ~unpacked:[ ("account_id", `Int 10) ]
      ([], [ (`M "id", Value.of_int 1) ])
  in
  print_sql_param sql param;

  let sql, param =
    Sql.insert ~table_name:"notifications" ~columns:[ "account_id" ]
      ~unpacked:[ ("account_id", `Int 10) ]
  in
  print_sql_param sql param;

  let sql, param = Sql.delete ~table_name:"notifications" ~id:10 in
  print_sql_param sql param;

  Db.debug_drop_all_tables_in_db ();%lwt
  Db.e (fun c ->
      c#execute
        {|
CREATE TABLE accounts (
  id SERIAL PRIMARY KEY,
  username TEXT NOT NULL,
  domain TEXT,
  display_name TEXT NOT NULL,
  created_at TIMESTAMP WITHOUT TIME ZONE NOT NULL,
  updated_at TIMESTAMP WITHOUT TIME ZONE NOT NULL
  )|}
        [];%lwt
      c#execute
        {|
CREATE TABLE notifications (
  id SERIAL PRIMARY KEY,
  activity_id BIGINT NOT NULL,
  activity_type TEXT NOT NULL,
  created_at TIMESTAMP WITHOUT TIME ZONE NOT NULL,
  updated_at TIMESTAMP WITHOUT TIME ZONE NOT NULL,
  account_id BIGINT NOT NULL,
  from_account_id BIGINT NOT NULL,
  type TEXT NOT NULL,

  FOREIGN KEY (account_id) REFERENCES accounts (id) ON DELETE CASCADE,
  FOREIGN KEY (from_account_id) REFERENCES accounts (id) ON DELETE CASCADE
  )|}
        []);%lwt

  let%lwt [ a1'; a2' ] =
    Db.e
      Account.(
        insert
          [
            new t ~username:"user1" ~display_name:"User 1" ();
            new t ~username:"user2" ~display_name:"User 2" ();
          ])
    [@@warning "-8"]
  in
  ignore a1'#id;
  ignore a2'#id;
  ignore a1'#created_at;
  ignore a1'#updated_at;
  ignore a2'#created_at;
  ignore a2'#updated_at;

  let%lwt [ a1 ] = Db.e Account.(select ~id:(`Eq a1'#id)) [@@warning "-8"] in
  let%lwt [ a2 ] = Db.e Account.(select ~id:(`Eq a2'#id)) [@@warning "-8"] in
  let%lwt a12 = Db.e Account.(select ~id:(`In [ a1#id; a2#id ])) in

  assert (a1#id = a1'#id);
  assert (a2#id = a2'#id);
  assert ([ a1#id; a2#id ] = (a12 |> List.map (fun a -> a#id)));
  assert (a1#username = "user1");
  assert (a2#username = "user2");

  let%lwt [ a1'; a2' ] =
    Db.e
      Account.(update [ a1#with_username "foo"; a2#with_domain (Some "bar") ])
  in
  assert (a1#created_at = a1'#created_at);
  assert (a2#created_at = a2'#created_at);
  assert (Ptime.is_earlier a1#updated_at ~than:a1'#updated_at);
  assert (Ptime.is_earlier a2#updated_at ~than:a2'#updated_at);
  assert (a1'#username = "foo");
  assert (a2'#domain = Some "bar");

  let%lwt [ n1'; n2' ] =
    Db.e
      Notification.(
        insert
          [
            new t
              ~activity_id:1 ~activity_type:`Status ~account_id:a1#id
              ~from_account_id:a2#id ~typ:`reblog ();
            new t
              ~activity_id:1 ~activity_type:`Status ~account_id:a2#id
              ~from_account_id:a1#id ~typ:`reblog ();
          ])
    [@@warning "-8"]
  in
  let%lwt [ n1; n2 ] =
    Db.e
      Notification.(select ~id:(`In [ n1'#id; n2'#id ]) ~preload:[ `account ])
  in

  assert (n1'#id = n1#id);
  assert (n1#account#id = a1#id);
  assert (n1#typ = Some `reblog);
  assert (n2'#id = n2#id);
  assert (n2#account#id = a2#id);

  let hook_called = ref false in
  Account.Internal.after_create_commit_callbacks :=
    [ (fun _r _c -> Lwt.return (hook_called := true)) ];

  let a_id = ref (Account.ID.of_int 0) in
  let%lwt res =
    Db.transaction (fun c ->
        Account.(insert [ new t ~username:"user3" ~display_name:"User 3" () ]) c
        >|= fun [ x ] -> a_id := x#id)
  in
  assert res;
  let%lwt a = Db.e Account.(select ~id:(`Eq !a_id)) in
  assert (List.length a = 1);
  assert !hook_called;

  hook_called := false;
  let%lwt res =
    Db.transaction (fun c ->
        ( Account.(insert [ new t ~username:"user4" ~display_name:"User 4" () ])
            c
        >|= fun [ x ] -> a_id := x#id );%lwt
        failwith "" (* Incur rollback *))
  in
  assert (not res);
  let%lwt a = Db.e Account.(select ~id:(`Eq !a_id)) in
  assert (List.length a = 0);
  assert (not !hook_called);

  Lwt.return_unit

let () =
  Logq.(add_reporter (make_reporter ~l:Debug ()));
  Db.initialize (Sys.getenv "SQLX_TEST_DB_URL");
  Lwt_main.run
  @@ Alcotest_lwt.run "sqlx"
       [ ("foo", [ Alcotest_lwt.test_case "case1" `Quick foo ]) ]
