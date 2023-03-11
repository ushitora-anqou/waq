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
end

module Status = struct
  [%%sqlx.schema
  name "statuses"

  val text : string
  val account_id : Account.ID.t
  val in_reply_to_id : ID.t option
  val reblog_of_id : ID.t option]
end

module Favourite = struct
  [%%sqlx.schema
  name "favourites"

  val status_id : Status.ID.t
  val account_id : Account.ID.t]
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
  val typ : typ_t option [@@column "type"]
  val target_status : Status.t [@@not_column]]

  let load_target_status (ns : t list) c =
    let map_fst_sort_uniq r = r |> List.map fst |> List.sort_uniq compare in

    (* Load favourites *)
    ( ns
    |> List.filter_map (fun n ->
           match (n#activity_type, n#typ) with
           | `Favourite, _ | _, `favourite ->
               Some (Favourite.ID.of_int n#activity_id, n#set_target_status)
           | _ -> None)
    |> fun r ->
      Favourite.select ~id:(`In (map_fst_sort_uniq r)) c
      >|= List.map (fun x -> (x#status_id, List.assoc x#id r))
      >>= fun r ->
      Status.select ~id:(`In (map_fst_sort_uniq r)) c
      >|= List.iter (fun x -> (List.assoc x#id r) x) );%lwt

    (* Load reblogs *)
    ( ns
    |> List.filter_map (fun n ->
           match (n#activity_type, n#typ) with
           | `Status, _ | _, `reblog ->
               Some (Status.ID.of_int n#activity_id, n#set_target_status)
           | _ -> None)
    |> fun r ->
      Status.select ~id:(`In (map_fst_sort_uniq r)) c
      >|= List.iter (fun x -> (List.assoc x#id r) x) );%lwt

    Lwt.return_unit
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
CREATE TABLE statuses (
  id SERIAL PRIMARY KEY,
  created_at TIMESTAMP WITHOUT TIME ZONE NOT NULL,
  updated_at TIMESTAMP WITHOUT TIME ZONE NOT NULL,
  text TEXT NOT NULL,
  account_id BIGINT NOT NULL,
  in_reply_to_id BIGINT,
  reblog_of_id BIGINT,

  FOREIGN KEY (account_id) REFERENCES accounts (id) ON UPDATE CASCADE ON DELETE CASCADE,
  FOREIGN KEY (in_reply_to_id) REFERENCES statuses (id) ON DELETE SET NULL,
  FOREIGN KEY (reblog_of_id) REFERENCES statuses (id) ON DELETE CASCADE
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
    [];%lwt
  c#execute
    {|
CREATE TABLE favourites (
  id SERIAL PRIMARY KEY,
  created_at TIMESTAMP WITHOUT TIME ZONE NOT NULL,
  updated_at TIMESTAMP WITHOUT TIME ZONE NOT NULL,
  account_id BIGINT NOT NULL,
  status_id BIGINT NOT NULL,

  FOREIGN KEY (account_id) REFERENCES accounts(id) ON DELETE CASCADE,
  FOREIGN KEY (status_id) REFERENCES statuses(id) ON DELETE CASCADE,
  UNIQUE (account_id, status_id)
)|}
    []

let test_account_basic_ops_case1 _ _ =
  setup1 ();%lwt
  let%lwt [ a1'; a2' ] =
    Db.e
      Account.(
        insert
          [
            make ~username:"user1" ~display_name:"User 1" ();
            make ~username:"user2" ~display_name:"User 2" ~domain:"example.com"
              ();
          ])
  in
  ignore a1'#id;
  ignore a2'#id;
  ignore a1'#created_at;
  ignore a1'#updated_at;
  ignore a2'#created_at;
  ignore a2'#updated_at;
  assert (a1'#username = "user1");
  assert (a2'#username = "user2");
  assert (a1'#domain_opt = None);
  assert (a2'#domain_opt = Some "example.com");

  let%lwt [ a1 ] = Db.e Account.(select ~id:(`Eq a1'#id)) in
  let%lwt [ a2 ] = Db.e Account.(select ~id:(`Eq a2'#id)) in
  let%lwt a12 = Db.e Account.(select ~id:(`In [ a1#id; a2#id ])) in
  assert (a1#id = a1'#id);
  assert (a2#id = a2'#id);
  assert (a1#id <> a2#id);
  assert (
    List.sort compare [ a1#id; a2#id ]
    = List.sort compare (a12 |> List.map (fun a -> a#id)));
  assert (a1#username = "user1");
  assert (a2#username = "user2");
  assert (a1#domain_opt = None);
  assert (a2#domain_opt = Some "example.com");

  let%lwt [ a2' ] = Db.e Account.(select ~domain:(`Eq "example.com")) in
  assert (a2#id = a2'#id);
  let%lwt [ a2' ] = Db.e Account.(select ~domain:`NeqNone) in
  assert (a2#id = a2'#id);
  let%lwt [ a1' ] = Db.e Account.(select ~domain:`EqNone) in
  assert (a1#id = a1'#id);

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

  (Db.e Account.(select ~id:(`Eq a1#id)) >|= fun [ x ] -> assert (x#id = a1#id));%lwt
  Db.e Account.(delete [ a1 ]);%lwt
  (Db.e Account.(select ~id:(`Eq a1#id)) >|= fun r -> assert (r = []));%lwt

  Lwt.return_unit
  [@@warning "-8"]

let test_status_preload _ _ =
  setup1 ();%lwt

  let%lwt [ a1; a2 ] =
    Db.e
      Account.(
        insert
          [
            make ~username:"user1" ~display_name:"User 1" ();
            make ~username:"user2" ~display_name:"User 2" ~domain:"example.com"
              ();
          ])
  in

  let%lwt [ s1 ] =
    Db.e Status.(insert [ make ~account_id:a1#id ~text:"foo" () ])
  in
  let%lwt [ s2 ] =
    Db.e
      Status.(
        insert [ make ~account_id:a2#id ~text:"bar" ~in_reply_to_id:s1#id () ])
  in
  let%lwt [ s3 ] =
    Db.e
      Status.(
        insert [ make ~account_id:a1#id ~text:"baz" ~reblog_of_id:s2#id () ])
  in
  assert (s1#account#id = a1#id);
  assert (s2#account#id = a2#id);
  assert (s3#account#id = a1#id);
  assert (s2#in_reply_to#id = s1#id);
  assert (s3#reblog_of#id = s2#id);

  let%lwt [ s3'; s2'; s1' ] =
    Db.e
      Status.(
        select ~id:(`In [ s1#id; s2#id; s3#id ]) ~order_by:[ (`id, `DESC) ])
  in
  assert (s1#id = s1'#id);
  assert (s2#id = s2'#id);
  assert (s3#id = s3'#id);
  assert (s1'#account#id = a1#id);
  assert (s2'#account#id = a2#id);
  assert (s3'#account#id = a1#id);
  assert (s2'#in_reply_to#id = s1#id);
  assert (s3'#reblog_of#id = s2#id);

  let%lwt [ s1 ] = Db.e Status.(select ~id:(`Eq s1#id) ~preload:[]) in
  assert (Option.is_none s1#in_reply_to_opt);
  assert (Option.is_none s1#reblog_of_opt);
  let%lwt [ s1' ] = Db.e Status.(update [ s1#with_text "foo modified" ]) in
  assert (s1#id = s1'#id);
  assert (s1'#text <> s1#text);
  assert (Ptime.is_earlier s1#updated_at ~than:s1'#updated_at);

  Lwt.return_unit
  [@@warning "-8"]

let test_notification_target_status _ _ =
  setup1 ();%lwt
  let%lwt [ a1; a2 ] =
    Db.e
      Account.(
        insert
          [
            make ~username:"user1" ~display_name:"User 1" ();
            make ~username:"user2" ~display_name:"User 2" ();
          ])
  in
  let%lwt [ s1 ] =
    Db.e Status.(insert [ make ~account_id:a1#id ~text:"foo" () ])
  in
  let%lwt [ s2 ] =
    Db.e
      Status.(insert [ make ~account_id:a2#id ~text:"" ~reblog_of_id:s1#id () ])
  in

  let%lwt [ n1 ] =
    Db.e
      Notification.(
        insert
          [
            make ~activity_id:(Status.ID.to_int s2#id) ~activity_type:`Status
              ~account_id:a1#id ~from_account_id:a2#id ~typ:`reblog ();
          ])
    [@@warning "-8"]
  in
  Db.e (Notification.load_target_status [ n1 ]);%lwt
  assert (n1#target_status#id = s2#id);
  assert (n1#target_status#reblog_of#id = s1#id);
  assert (n1#target_status#reblog_of#text = "foo");

  let%lwt [ f1 ] =
    Db.e Favourite.(insert [ make ~status_id:s1#id ~account_id:a2#id () ])
  in
  let%lwt [ n2 ] =
    Db.e
      Notification.(
        insert
          [
            make
              ~activity_id:(Favourite.ID.to_int f1#id)
              ~activity_type:`Favourite ~account_id:a1#id ~from_account_id:a2#id
              ~typ:`favourite ();
          ])
  in
  Db.e (Notification.load_target_status [ n2 ]);%lwt
  assert (n2#target_status#id = s1#id);

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

let test_count_status _ _ =
  setup1 ();%lwt
  let%lwt [ a1; a2 ] =
    Db.e
      Account.(
        insert
          [
            make ~username:"user1" ~display_name:"User 1" ();
            make ~username:"user2" ~display_name:"User 2" ~domain:"example.com"
              ();
          ])
  in
  let%lwt [ s1 ] =
    Db.e Status.(insert [ make ~account_id:a1#id ~text:"foo" () ])
  in
  let%lwt [ s2 ] =
    Db.e
      Status.(
        insert [ make ~account_id:a2#id ~text:"bar" ~in_reply_to_id:s1#id () ])
  in
  let%lwt [ _ ] =
    Db.e
      Status.(
        insert [ make ~account_id:a1#id ~text:"baz" ~reblog_of_id:s2#id () ])
  in

  let%lwt statuses_count = Db.e Status.(count) in
  assert (statuses_count = 3);

  let%lwt reblogs_count = Db.e Status.(count ~reblog_of_id:(`Eq s1#id)) in
  assert (reblogs_count = 0);
  let%lwt reblogs_count = Db.e Status.(count ~reblog_of_id:(`Eq s2#id)) in
  assert (reblogs_count = 1);

  let%lwt replies_count = Db.e Status.(count ~in_reply_to_id:(`Eq s1#id)) in
  assert (replies_count = 1);
  let%lwt replies_count = Db.e Status.(count ~in_reply_to_id:(`Eq s2#id)) in
  assert (replies_count = 0);

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
             Alcotest_lwt.test_case "account" `Quick
               test_account_basic_ops_case1;
           ] );
         ( "preload",
           [
             Alcotest_lwt.test_case "status" `Quick test_status_preload;
             Alcotest_lwt.test_case "notification" `Quick
               test_notification_target_status;
           ] );
         ( "transaction",
           [ Alcotest_lwt.test_case "case1" `Quick test_transaction_case1 ] );
         ("count", [ Alcotest_lwt.test_case "status" `Quick test_count_status ]);
       ]
