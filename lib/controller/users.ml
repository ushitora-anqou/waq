open Util
open Activity

(* Recv GET /users/:name *)
let get req =
  let username = req |> Httpq.Server.param ":name" in
  try%lwt
    let%lwt a = Db.Account.get_one ~domain:None ~username () in
    let%lwt _ = Db.User.get_one ~account_id:a.id () in
    let publicKey =
      make_ap_user_public_key ~id:(a.uri ^ "#main-key") ~owner:a.uri
        ~publicKeyPem:a.public_key
    in
    make_ap_user ~context ~id:a.uri ~typ:"Person"
      ~following:(a.uri ^/ "following") ~followers:a.followers_url
      ~inbox:a.inbox_url ~outbox:(a.uri ^/ "outbox") ~preferredUsername:username
      ~name:a.display_name ~summary:"Summary is here" ~url:a.uri ~tag:[]
      ~publicKey ()
    |> ap_user_to_yojson |> Yojson.Safe.to_string
    |> Httpq.Server.respond ~headers:[ Helper.content_type_app_jrd_json ]
  with e ->
    Logq.debug (fun m ->
        m "[get_users] Can't find user: %s: %s\n%s" username
          (Printexc.to_string e)
          (Printexc.get_backtrace ()));
    Httpq.Server.raise_error_response `Not_found
