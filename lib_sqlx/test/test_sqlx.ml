open Lwt.Infix [@@warning "-33"]
open Sqlx

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
  Db.do_query (fun c ->
      Internal.execute c
        {|
CREATE TABLE accounts (
  id SERIAL PRIMARY KEY,
  username TEXT NOT NULL,
  domain TEXT,
  display_name TEXT NOT NULL,
  created_at TIMESTAMP WITHOUT TIME ZONE NOT NULL,
  updated_at TIMESTAMP WITHOUT TIME ZONE NOT NULL
)|};%lwt
      Internal.execute c
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
  )|});%lwt

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

  Lwt.return_unit

let () =
  Logq.(add_reporter (make_reporter ~l:Debug ()));
  Db.initialize (Sys.getenv "SQLX_TEST_DB_URL");
  Lwt_main.run
  @@ Alcotest_lwt.run "sqlx"
       [ ("foo", [ Alcotest_lwt.test_case "case1" `Quick foo ]) ]
