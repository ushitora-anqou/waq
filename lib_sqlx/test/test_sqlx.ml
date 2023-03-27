open Lwt.Infix [@@warning "-33"]
open Sqlx
open Util

[@@@warning "-32-39"]

[%%sqlx.schemas
module rec Account = struct
  name "accounts"

  class type t =
    object
      val username : string
      val domain : string option
      val display_name : string
    end
end

and Status = struct
  name "statuses"

  class type t =
    object
      val text : string
      val in_reply_to_id : ID.t option
      val reblog_of_id : ID.t option
      val account_id : Account.ID.t
    end
end

and Favourite = struct
  name "favourites"

  class type t =
    object
      val account_id : Account.ID.t
      val status_id : Status.ID.t
    end
end

and Notification = struct
  name "notifications"

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

  class type t =
    object
      val activity_id : int
      val activity_type : activity_type_t
      val account_id : Account.ID.t
      val from_account_id : Account.ID.t
      val typ : typ_t option [@@column "type"]

      val target_status : Status.t
      [@@not_column] [@@preload_spec: Status.preload_spec]
    end
end]

module Notification = struct
  include Notification

  let () =
    loader_target_status :=
      fun ?preload ns c ->
        let map_fst_sort_uniq r = r |> List.map fst |> List.sort_uniq compare in

        (* Load favourites *)
        ( ns
        |> List.filter_map (fun n ->
               match (n#activity_type, n#typ) with
               | `Favourite, _ | _, Some `favourite ->
                   Some (Favourite.ID.of_int n#activity_id, n#set_target_status)
               | _ -> None)
        |> fun r ->
          Favourite.select ~id:(`In (map_fst_sort_uniq r)) c
          >|= List.map (fun x -> (x#status_id, List.assoc x#id r))
          >>= fun r ->
          Status.select ?preload ~id:(`In (map_fst_sort_uniq r)) c
          >|= List.iter (fun x -> (List.assoc x#id r) (Some x)) );%lwt

        (* Load reblogs *)
        ( ns
        |> List.filter_map (fun n ->
               match (n#activity_type, n#typ) with
               | `Status, _ | _, Some `reblog ->
                   Some (Status.ID.of_int n#activity_id, n#set_target_status)
               | _ -> None)
        |> fun r ->
          Status.select ?preload ~id:(`In (map_fst_sort_uniq r)) c
          >|= List.iter (fun x -> (List.assoc x#id r) (Some x)) );%lwt

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
)|};%lwt
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
)|};%lwt
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
)|};%lwt
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
  assert (a1'#domain = None);
  assert (a2'#domain = Some "example.com");

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
  assert (a1#domain = None);
  assert (a2#domain = Some "example.com");

  let%lwt [ a2' ] = Db.e Account.(select ~domain:(`Eq "example.com")) in
  assert (a2#id = a2'#id);
  let%lwt [ a2' ] = Db.e Account.(select ~domain:`NeqNone) in
  assert (a2#id = a2'#id);
  let%lwt [ a1' ] = Db.e Account.(select ~domain:`EqNone) in
  assert (a1#id = a1'#id);

  let%lwt _ =
    Db.e
      Account.(
        select
          ~where:(`Or (`Eq (`username, `_name1), `Eq (`username, `_name2)))
          ~p:[ (`_name1, `String "user1"); (`_name2, `String "user2") ])
    >|= List.map (fun a -> a#id)
    >|= List.sort compare
    >|= fun ids -> assert (ids = List.sort compare [ a1#id; a2#id ])
  in
  let%lwt _ =
    Db.e Account.(select ~username:(`Eq "user1") ~where:(`IsNull `domain))
    >|= fun [ a ] -> assert (a#id = a1#id)
  in

  let%lwt _ =
    Db.e Account.all >|= fun accts ->
    assert (
      accts
      |> List.map (fun a -> a#id)
      |> List.sort compare
      = ([ a1'#id; a2'#id ] |> List.sort compare))
  in

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
    Db.e
      Status.(
        insert
          [ make ~account_id:a1#id ~text:"foo" () ]
          ~preload:[ `account [] ])
  in
  let%lwt [ s2 ] =
    Db.e
      Status.(
        insert
          [ make ~account_id:a2#id ~text:"bar" ~in_reply_to_id:s1#id () ]
          ~preload:[ `account []; `in_reply_to [] ])
  in
  let%lwt [ s3 ] =
    Db.e
      Status.(
        insert
          [ make ~account_id:a1#id ~text:"baz" ~reblog_of_id:s2#id () ]
          ~preload:[ `account []; `reblog_of [] ])
  in
  assert (s1#account#id = a1#id);
  assert (s2#account#id = a2#id);
  assert (s3#account#id = a1#id);
  assert ((Option.get s2#in_reply_to)#id = s1#id);
  assert ((Option.get s3#reblog_of)#id = s2#id);

  let%lwt [ s3'; s2'; s1' ] =
    Db.e
      Status.(
        select
          ~id:(`In [ s1#id; s2#id; s3#id ])
          ~order_by:[ (`id, `DESC) ]
          ~preload:[ `account []; `in_reply_to []; `reblog_of [] ])
  in
  assert (s1#id = s1'#id);
  assert (s2#id = s2'#id);
  assert (s3#id = s3'#id);
  assert (s1'#account#id = a1#id);
  assert (s2'#account#id = a2#id);
  assert (s3'#account#id = a1#id);
  assert ((Option.get s2'#in_reply_to)#id = s1#id);
  assert ((Option.get s3'#reblog_of)#id = s2#id);

  let%lwt [ s1 ] =
    Db.e
      Status.(
        select ~id:(`Eq s1#id) ~preload:[ `in_reply_to []; `reblog_of [] ])
  in
  assert (Option.is_none s1#in_reply_to);
  assert (Option.is_none s1#reblog_of);
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
          ]
          ~preload:[ `target_status [ `reblog_of [] ] ])
    [@@warning "-8"]
  in
  assert (n1#target_status#id = s2#id);
  assert ((Option.get n1#target_status#reblog_of)#id = s1#id);
  assert ((Option.get n1#target_status#reblog_of)#text = "foo");

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
          ]
          ~preload:[ `target_status [] ])
  in
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

let test_account_derived_ops _ _ =
  setup1 ();%lwt

  let%lwt a1 =
    Db.e Account.(save_one (make ~username:"user1" ~display_name:"foo" ()))
  in

  let%lwt _ =
    Db.e Account.(get_one ~username:"user1") >|= fun a ->
    assert (a#id = a1#id);
    assert (a#display_name = "foo")
  in
  let%lwt _ =
    Db.e Account.(get_one ~domain:None) >|= fun a -> assert (a#id = a1#id)
  in

  let%lwt a1 = Db.e Account.(save_one (a1#with_display_name "mod")) in
  assert (a1#display_name = "mod");
  let%lwt _ =
    Db.e Account.(get_one ~username:"user1") >|= fun a ->
    assert (a#display_name = "mod")
  in

  let%lwt a2 =
    Db.e Account.(save_one (make ~username:"user2" ~display_name:"bar" ()))
  in
  let%lwt _ =
    Db.e Account.(get_many ~username:"user2") >|= function
    | [ a ] -> assert (a#id = a2#id)
  in

  Lwt.return_unit
  [@@warning "-8"]

module M1 : Migration.S = struct
  let up (c : Sqlx.Connection.t) : unit Lwt.t =
    c#execute {|CREATE TABLE t1 ( col1 INTEGER )|}

  let down (c : Sqlx.Connection.t) : unit Lwt.t = c#execute {|DROP TABLE t1|}
end

module M2 : Migration.S = struct
  let up (c : Sqlx.Connection.t) : unit Lwt.t =
    c#execute {|CREATE TABLE t2 ( col1 INTEGER )|}

  let down (c : Sqlx.Connection.t) : unit Lwt.t = c#execute {|DROP TABLE t2|}
end

let expect_exc_lwt f =
  try%lwt
    f |> ignore_lwt;%lwt
    assert false
  with _ -> Lwt.return_unit

let test_migration _ _ =
  let open Migration in
  let config = { schema_migrations = "schema_migrations" } in

  Db.debug_drop_all_tables_in_db ();%lwt
  let migrations = [ (1, (module M1 : S)) ] in
  Db.e (migrate ~config ~migrations);%lwt
  let%lwt [] = Db.e (fun c -> c#query "SELECT * FROM t1") in
  Db.e (rollback ~n:1 ~config ~migrations);%lwt
  expect_exc_lwt (Db.e (fun c -> c#query "SELECT * FROM t1"));%lwt
  Db.e (migrate ~config ~migrations);%lwt

  Db.debug_drop_all_tables_in_db ();%lwt
  let migrations = [ (1, (module M1 : S)); (2, (module M2 : S)) ] in
  Db.e (migrate ~config ~migrations);%lwt
  let%lwt [] = Db.e (fun c -> c#query "SELECT * FROM t1") in
  let%lwt [] = Db.e (fun c -> c#query "SELECT * FROM t2") in
  Db.e (rollback ~n:2 ~config ~migrations);%lwt
  expect_exc_lwt (Db.e (fun c -> c#query "SELECT * FROM t1"));%lwt
  expect_exc_lwt (Db.e (fun c -> c#query "SELECT * FROM t2"));%lwt
  Db.e (migrate ~config ~migrations);%lwt

  Db.debug_drop_all_tables_in_db ();%lwt
  let migrations =
    [ (1, (module M1 : S)); (2, (module M1 : S)); (3, (module M2 : S)) ]
  in
  expect_exc_lwt (Db.e (migrate ~config ~migrations));%lwt
  let%lwt [] = Db.e (fun c -> c#query "SELECT * FROM t1") in
  expect_exc_lwt (Db.e (fun c -> c#query "SELECT * FROM t2"));%lwt

  Db.debug_drop_all_tables_in_db ();%lwt
  Db.e (migrate ~config ~migrations:[ (1, (module M1 : S)) ]);%lwt
  expect_exc_lwt (Db.e (migrate ~config ~migrations:[ (2, (module M2 : S)) ]));%lwt

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
         ( "*_one, *_many",
           [ Alcotest_lwt.test_case "account" `Quick test_account_derived_ops ]
         );
         ( "migrate",
           [ Alcotest_lwt.test_case "migration" `Quick test_migration ] );
       ]
