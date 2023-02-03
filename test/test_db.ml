open Waq
open Db

let test_user _ _ =
  initialize ();
  (try%lwt Migration.rollback () with _ -> Lwt.return_unit);%lwt
  Migration.migrate ();%lwt

  let created_at, _, _ =
    Ptime.of_rfc3339 "2022-12-31T21:03:07.4242+09:00" |> Result.get_ok
  in
  let updated_at, _, _ =
    Ptime.of_rfc3339 "2023-12-31T21:03:07.4242+09:00" |> Result.get_ok
  in

  let%lwt a =
    Account.make ~username:"anqou" ~public_key:"" ~display_name:"Ushitora Anqou"
      ~uri:"" ~inbox_url:"" ~followers_url:"" ~created_at ~updated_at ()
    |> Account.save_one
  in
  assert (a.id = 1);
  assert (a.username = "anqou");
  assert (a.private_key = None);
  assert (a.public_key = "");
  assert (a.display_name = "Ushitora Anqou");
  assert (a.created_at = created_at);
  assert (a.updated_at = updated_at);

  let%lwt a = Account.get_one ~id:1 () in
  assert (a.id = 1);
  assert (a.username = "anqou");
  assert (a.private_key = None);
  assert (a.public_key = "");
  assert (a.display_name = "Ushitora Anqou");
  assert (a.created_at = created_at);
  assert (a.updated_at = updated_at);

  let%lwt u =
    User.make ~id:0 ~email:"ushitora@anqou.net" ~created_at ~updated_at
      ~account_id:1
    |> User.save_one
  in
  assert (u.id = 1);
  assert (u.email = "ushitora@anqou.net");
  assert (u.created_at = created_at);
  assert (u.updated_at = updated_at);
  assert (u.account_id = 1);

  let%lwt a = Account.get_one ~username:"anqou" () in
  let%lwt u = User.get_one ~account_id:a.id () in
  assert (u.id = 1);
  assert (u.email = "ushitora@anqou.net");
  assert (u.created_at = created_at);
  assert (u.updated_at = updated_at);
  assert (u.account_id = 1);

  let%lwt u = User.get_one ~id:1 () in
  assert (u.id = 1);
  assert (u.email = "ushitora@anqou.net");

  Lwt.return_unit

let () =
  Logq.(add_reporter (make_reporter ~l:Debug ()));
  Config.load_string {|db_url: "postgresql://anqou@localhost:5432/waq_dev"|};
  Lwt_main.run
  @@ Alcotest_lwt.run "db"
       [ ("user", [ Alcotest_lwt.test_case "insert & get" `Quick test_user ]) ]
