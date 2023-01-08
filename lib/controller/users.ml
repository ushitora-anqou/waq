open Common
open Activity

(* Recv GET /users/:name *)
let get username =
  try%lwt
    let%lwt u = Db.get_user_by_username username in
    let%lwt a = Db.get_account ~id:u.account_id in
    let publicKey =
      make_ap_user_public_key ~id:(a.uri ^ "#main-key") ~owner:a.uri
        ~publicKeyPem:a.public_key
    in
    make_ap_user ~context ~id:a.uri ~typ:"Person"
      ~following:(a.uri ^/ "following") ~followers:a.followers_url
      ~inbox:a.inbox_url ~outbox:(a.uri ^/ "outbox") ~preferredUsername:username
      ~name:a.display_name ~summary:"Summary is here" ~url:a.uri ~tag:[]
      ~publicKey ()
    |> ap_user_to_yojson |> Yojson.Safe.to_string |> Result.ok |> Lwt.return
  with e ->
    Log.debug (fun m ->
        m "[get_users] Can't find user: %s: %s\n%s" username
          (Printexc.to_string e)
          (Printexc.get_backtrace ()));
    Lwt.return (Error `Not_found)
