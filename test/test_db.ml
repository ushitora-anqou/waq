open Waq
open Db

let test_user _ _ =
  initialize ();
  (try%lwt rollback () with _ -> Lwt.return_unit);%lwt
  migrate ();%lwt

  let created_at, _, _ =
    Ptime.of_rfc3339 "2022-12-31T21:03:07.4242+09:00" |> Result.get_ok
  in
  let updated_at, _, _ =
    Ptime.of_rfc3339 "2023-12-31T21:03:07.4242+09:00" |> Result.get_ok
  in

  let%lwt a =
    make_account ~username:"anqou" ~public_key:"" ~display_name:"Ushitora Anqou"
      ~uri:"" ~inbox_url:"" ~followers_url:"" ~created_at ~updated_at ()
    |> insert_account
  in
  assert (a.id = 1);
  assert (a.username = "anqou");
  assert (a.private_key = None);
  assert (a.public_key = "");
  assert (a.display_name = "Ushitora Anqou");
  assert (a.created_at = created_at);
  assert (a.updated_at = updated_at);

  let%lwt a = get_account ~by:(`id 1) in
  assert (a.id = 1);
  assert (a.username = "anqou");
  assert (a.private_key = None);
  assert (a.public_key = "");
  assert (a.display_name = "Ushitora Anqou");
  assert (a.created_at = created_at);
  assert (a.updated_at = updated_at);

  let%lwt u =
    make_user ~id:0 ~email:"ushitora@anqou.net" ~created_at ~updated_at
      ~account_id:1
    |> insert_user
  in
  assert (u.id = 1);
  assert (u.email = "ushitora@anqou.net");
  assert (u.created_at = created_at);
  assert (u.updated_at = updated_at);
  assert (u.account_id = 1);

  let%lwt u = get_user ~by:(`username "anqou") in
  assert (u.id = 1);
  assert (u.email = "ushitora@anqou.net");
  assert (u.created_at = created_at);
  assert (u.updated_at = updated_at);
  assert (u.account_id = 1);

  Lwt.return_unit

let () =
  Log.initialize Debug;
  Config.load_string {|db_url: "postgresql://anqou@localhost:5432/waq_dev"|};
  Lwt_main.run
  @@ Alcotest_lwt.run "db"
       [ ("user", [ Alcotest_lwt.test_case "insert & get" `Quick test_user ]) ]
