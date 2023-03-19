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
  Migration.verify_migration_status ();%lwt
  Logq.info (fun m -> m "Listening on 127.0.0.1:%d" port);
  Lwt.return_unit

let db_migrate () = Lwt_main.run @@ Migration.migrate ()
let db_rollback () = Lwt_main.run @@ Migration.rollback ()

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
      let%lwt a = Db.(e @@ Account.get_one ~username) in
      Db.(e @@ User.get_one ~account_id:a#id)
    in
    let%lwt access_token =
      Oauth_helper.generate_access_token ~scopes:"read write follow push"
        ~resource_owner_id:user#id ~app:web
    in
    Lwt_io.printf "%s\n%!" access_token#token;%lwt
    Lwt.return_unit
  in
  Lwt_main.run f

let hidden_input f =
  let open Unix in
  let attr = tcgetattr stdin in
  attr.c_echo <- false;
  tcsetattr stdin TCSAFLUSH attr;
  let res = f () in
  attr.c_echo <- true;
  tcsetattr stdin TCSAFLUSH attr;
  res

let user_register () =
  let f =
    print_string "Username: ";
    let username = read_line () |> String.trim in
    print_string "Display name: ";
    let display_name = read_line () |> String.trim in
    print_string "Email: ";
    let email = read_line () |> String.trim in
    let password =
      hidden_input @@ fun () ->
      print_string "Password: ";
      let password = read_line () |> String.trim in
      print_newline ();
      print_string "Retype your password: ";
      let password' = read_line () |> String.trim in
      print_newline ();
      if password <> password' then failwith "Password was not equal";
      password
    in
    let%lwt _, u = Db.register_user ~username ~display_name ~email ~password in
    Printf.printf "Correctly registered. User # = %d"
      (u#id |> Model.User.ID.to_int);
    Lwt.return_unit
  in
  Lwt_main.run f

let account_fetch () =
  let open Lwt.Infix in
  let f =
    let%lwt accts = Db.(e Account.all) in
    accts
    |> Lwt_list.iteri_s @@ fun i acct ->
       Logq.info (fun m -> m "[%d/%d] %s" (i + 1) (List.length accts) acct#uri);
       match acct#domain with
       | None -> Lwt.return_unit
       | Some domain -> (
           try%lwt
             Activity.fetch_person (`DomainUser (domain, acct#username))
             >|= Activity.model_account_of_person ~original:acct
             >>= Db.(e *< Account.save_one)
             |> ignore_lwt
           with e ->
             Logq.err (fun m ->
                 m "Couldn't fetch person: %s" (Printexc.to_string e));
             Lwt.return_unit)
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
  | "db:migrate" -> db_migrate ()
  | "db:rollback" -> db_rollback ()
  | "oauth:generate_access_token" -> oauth_generate_access_token Sys.argv.(2)
  | "user:register" -> user_register ()
  | "account:fetch" -> account_fetch ()
  | _ -> server ()
