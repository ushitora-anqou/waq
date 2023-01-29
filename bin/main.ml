open Waq
open Util [@@warning "-33"]
module C = Config

let register_user ~username ~display_name ~email =
  let now = Ptime.now () in
  let created_at, updated_at = (now, now) in
  let private_key, public_key = Http.Signature.generate_keypair () in
  let public_key = Http.Signature.encode_public_key public_key in
  let private_key = Http.Signature.encode_private_key private_key in
  let uri = Activity.url [ "users"; username ] in
  let inbox_url = Activity.(uri ^/ "inbox") in
  let followers_url = Activity.(uri ^/ "followers") in
  let%lwt a =
    Db.Account.(
      make ~username ~public_key ~private_key ~display_name ~uri ~inbox_url
        ~followers_url ~created_at ~updated_at ()
      |> save_one)
  in
  let%lwt u =
    let created_at, updated_at = (now, now) in
    Db.User.(
      make ~id:0 ~email ~created_at ~updated_at ~account_id:a.id |> save_one)
  in
  Lwt.return (a, u)

let server () =
  let _host, port = (C.listen_host (), C.listen_port ()) in
  Router.routes
  |> Http.start_server ~port @@ fun () ->
     Log.info (fun m -> m "Listening on 127.0.0.1:%d" port);
     Lwt.return_unit

let db_reset () =
  let f : unit Lwt.t =
    Db.debug_drop_all_tables_in_db ();%lwt
    Migration.migrate ();%lwt

    (* Generate some users for tests.
       FIXME: Should be removed once an endpoint to register a user is implemented *)
    [ 1; 2; 3 ] |> List.rev
    |> Lwt_list.iter_s (fun i ->
           let open Printf in
           let username = sprintf "user%d" i in
           let display_name = sprintf "User %d's display name" i in
           let email = sprintf "user%d@example.com" i in
           register_user ~username ~display_name ~email |> ignore_lwt)
  in
  Lwt_main.run f

let oauth_generate_access_token username =
  let f =
    (* Generate a new OAuth application named "Web" *)
    let%lwt web =
      Oauth.generate_application ~name:"Web"
        ~redirect_uri:"urn:ietf:wg:oauth:2.0:oob"
        ~scopes:"read write follow push"
    in
    let%lwt user =
      let%lwt a = Db.Account.get_one ~username () in
      Db.User.get_one ~account_id:a.id ()
    in
    let%lwt access_token =
      Oauth.generate_access_token ~scopes:"read write follow push"
        ~resource_owner_id:user.id ~app:web
    in
    Lwt_io.printf "%s\n%!" access_token.token;%lwt
    Lwt.return_unit
  in
  Lwt_main.run f

let () =
  Log.(add_reporter (make_reporter ~l:Debug ()));
  C.load_file "config.yml";
  Crypto.initialize ();
  Db.initialize ();
  let subcommand =
    if Array.length Sys.argv < 2 then "server" else Sys.argv.(1)
  in
  match subcommand with
  | "db:reset" -> db_reset ()
  | "oauth:generate_access_token" -> oauth_generate_access_token Sys.argv.(2)
  | _ -> server ()
