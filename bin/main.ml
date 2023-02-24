open Waq
open Util [@@warning "-33"]

let server () =
  let _host, port = Config.(listen_host (), listen_port ()) in

  let error_handler ~req ~status ~headers ~body =
    let open Controller.Helper in
    let default () = Httpq.Server.respond ~status ~headers body in
    let main () =
      Httpq.Server.respond ~status ~headers
        ("<h1>" ^ Httpq.Status.to_string status ^ "</h1>")
    in
    req |> render ~default [ (text_html, main) ]
  in

  Httpq.Server.start_server ~port ~error_handler Router.handler @@ fun () ->
  Logq.info (fun m -> m "Listening on 127.0.0.1:%d" port);
  Lwt.return_unit

let db_reset () =
  let f : unit Lwt.t =
    Db.debug_drop_all_tables_in_db ();%lwt
    Migration.migrate ();%lwt

    if Config.debug_generate_test_users () then
      (* Generate some users for tests *)
      [ 1; 2; 3 ] |> List.rev
      |> Lwt_list.iter_s (fun i ->
             let open Printf in
             let username = sprintf "user%d" i in
             let display_name = sprintf "User %d's display name" i in
             let email = sprintf "user%d@example.com" i in
             let password = sprintf "user%dpassword" i in
             Db.register_user ~username ~display_name ~email ~password
             |> ignore_lwt)
    else Lwt.return_unit
  in
  Lwt_main.run f

let oauth_generate_access_token username =
  let f =
    (* Generate a new OAuth application named "Web" *)
    let%lwt web =
      Oauth_helper.generate_application ~name:"Web"
        ~redirect_uri:"urn:ietf:wg:oauth:2.0:oob"
        ~scopes:"read write follow push"
    in
    let%lwt user =
      let%lwt a = Db.Account.get_one ~username () in
      Db.User.get_one ~account_id:a.id ()
    in
    let%lwt access_token =
      Oauth_helper.generate_access_token ~scopes:"read write follow push"
        ~resource_owner_id:user.id ~app:web
    in
    Lwt_io.printf "%s\n%!" access_token.token;%lwt
    Lwt.return_unit
  in
  Lwt_main.run f

let () =
  Logq.(add_reporter (make_reporter ~l:Debug ()));
  Config.(load_file (config_path ()));
  Crypto.initialize ();
  Db.initialize ();
  let subcommand =
    if Array.length Sys.argv < 2 then "server" else Sys.argv.(1)
  in
  match subcommand with
  | "db:reset" -> db_reset ()
  | "oauth:generate_access_token" -> oauth_generate_access_token Sys.argv.(2)
  | _ -> server ()
