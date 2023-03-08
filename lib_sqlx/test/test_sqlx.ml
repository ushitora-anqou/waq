open Lwt.Infix [@@warning "-33"]
open Sqlx
open Util

[@@@warning "-32-39"]

module Account = struct
  [%%sqlx.schema
  name "accounts"

  val username : string
  val domain : string option
  val display_name : string]

  [%%sqlx.gen t]
end

module Status = struct
  [%%sqlx.schema
  name "statuses"

  val text : string
  val account_id : Account.ID.t

  (*
  val in_reply_to_id : ID.t
  val in_reply_to_id : ID.t option
  val reblog_of_id : ID.t option
  *)]

  [%%sqlx.gen t]
end

module Notification = struct
  (* v User defined functions *)
  type activity_type_t = [ `Status | `Favourite | `Follow ]

  let activity_type_t_to_string : activity_type_t -> string = function
    | `Status -> "Status"
    | `Favourite -> "Favourite"
    | `Follow -> "Follow"

  let activity_type_t_of_string : string -> activity_type_t = function
    | "Status" -> `Status
    | "Favourite" -> `Favourite
    | "Follow" -> `Follow
    | _ -> failwith "activity_type_t_of_string: invalid input"

  type typ_t = [ `reblog | `favourite | `follow ]

  let typ_t_to_string : typ_t -> string = function
    | `reblog -> "reblog"
    | `favourite -> "favourite"
    | `follow -> "follow"

  let typ_t_of_string : string -> typ_t = function
    | "reblog" -> `reblog
    | "favourite" -> `favourite
    | "follow" -> `follow
    | _ -> failwith "type_t_of_string: invalid input"

  (* ^ User defined functions *)

  [%%sqlx.schema
  name "notifications"

  val activity_id : int
  val activity_type : activity_type_t
  val account_id : Account.ID.t
  val from_account_id : Account.ID.t
  val typ : typ_t option [@@column "type"]]

  [%%sqlx.gen t]
end

module Db = struct
  include Engine.Make (Driver_pg)

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

let setup1 () =
  Db.debug_drop_all_tables_in_db ();%lwt
  Db.e @@ fun c ->
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
    []

let test_select_insert_update_delete_case1 _ _ =
  setup1 ();%lwt
  let%lwt [ a1'; a2' ] =
    Db.e
      Account.(
        insert
          [
            make ~username:"user1" ~display_name:"User 1" ();
            make ~username:"user2" ~display_name:"User 2" ();
          ])
  in
  ignore a1'#id;
  ignore a2'#id;
  ignore a1'#created_at;
  ignore a1'#updated_at;
  ignore a2'#created_at;
  ignore a2'#updated_at;

  let%lwt [ a1 ] = Db.e Account.(select ~id:(`Eq a1'#id)) in
  let%lwt [ a2 ] = Db.e Account.(select ~id:(`Eq a2'#id)) in
  let%lwt a12 = Db.e Account.(select ~id:(`In [ a1#id; a2#id ])) in

  assert (a1#id = a1'#id);
  assert (a2#id = a2'#id);
  assert ([ a1#id; a2#id ] = (a12 |> List.map (fun a -> a#id)));
  assert (a1#username = "user1");
  assert (a2#username = "user2");

  (*
  assert (a1#is_local && a2#is_local);
  *)
  let%lwt [ a1'; a2' ] =
    Db.e
      Account.(update [ a1#with_username "foo"; a2#with_domain (Some "bar") ])
  in
  assert (a1#created_at = a1'#created_at);
  assert (a2#created_at = a2'#created_at);
  assert (Ptime.is_earlier a1#updated_at ~than:a1'#updated_at);
  assert (Ptime.is_earlier a2#updated_at ~than:a2'#updated_at);
  assert (a1'#username = "foo");
  assert (a2'#domain = "bar");

  let%lwt [ n1'; n2' ] =
    Db.e
      Notification.(
        insert
          [
            make ~activity_id:1 ~activity_type:`Status ~account_id:a1#id
              ~from_account_id:a2#id ~typ:`reblog ();
            make ~activity_id:1 ~activity_type:`Status ~account_id:a2#id
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
  assert (n1#typ = `reblog);
  assert (n2'#id = n2#id);
  assert (n2#account#id = a2#id);

  (Db.e Account.(select ~id:(`Eq a1#id)) >|= fun [ x ] -> assert (x#id = a1#id));%lwt
  Db.e Account.(delete [ a1 ]);%lwt
  (Db.e Account.(select ~id:(`Eq a1#id)) >|= fun r -> assert (r = []));%lwt

  Lwt.return_unit
  [@@warning "-8"]

let test_transaction_case1 _ _ =
  let hook_called = ref false in
  Account.after_create_commit_callbacks :=
    [ (fun _r _c -> Lwt.return (hook_called := true)) ];

  let a_id = ref (Account.ID.of_int 0) in
  let%lwt res =
    Db.transaction (fun c ->
        Account.(insert [ make ~username:"user3" ~display_name:"User 3" () ]) c
        >|= fun [ x ] -> a_id := x#id)
  in
  assert res;
  let%lwt a = Db.e Account.(select ~id:(`Eq !a_id)) in
  assert (List.length a = 1);
  assert !hook_called;

  hook_called := false;
  let%lwt res =
    Db.transaction (fun c ->
        ( Account.(insert [ make ~username:"user4" ~display_name:"User 4" () ]) c
        >|= fun [ x ] -> a_id := x#id );%lwt
        failwith "" (* Incur rollback *))
  in
  assert (not res);
  let%lwt a = Db.e Account.(select ~id:(`Eq !a_id)) in
  assert (List.length a = 0);
  assert (not !hook_called);

  Lwt.return_unit
  [@@warning "-8"]

let () =
  Logq.(add_reporter (make_reporter ~l:Debug ()));
  Db.initialize (Sys.getenv "SQLX_TEST_DB_URL");
  Lwt_main.run
  @@ Alcotest_lwt.run "sqlx"
       [
         ( "select/insert/update/delete",
           [
             Alcotest_lwt.test_case "case1" `Quick
               test_select_insert_update_delete_case1;
           ] );
         ( "transaction",
           [ Alcotest_lwt.test_case "case1" `Quick test_transaction_case1 ] );
       ]
