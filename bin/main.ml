open Waq
open Util [@@warning "-33"]

let server env =
  let host, port = Config.(listen_host (), listen_port ()) in

  let error_handler ~req ~status ~headers ~body =
    let open Controller.Helper in
    let default () = Yume.Server.respond ~status ~headers body in
    let main () =
      Yume.Server.respond ~status ~headers
        ("<h1>" ^ Yume.Status.to_string status ^ "</h1>")
    in
    req |> render ~default [ (text_html, main) ]
  in

  Eio.Switch.run @@ fun sw ->
  Job.Runner.start_global_runner ~sw;
  let listen =
    match
      Eio.Net.getaddrinfo_stream ~service:(string_of_int port)
        (Eio.Stdenv.net env) host
    with
    | [] -> `Tcp (Eio.Net.Ipaddr.V4.loopback, 8080)
    | x :: _ -> x
  in
  Yume.Server.start_server env ~sw ~listen ~error_handler Router.handler
  @@ fun _ ->
  Migration.verify_migration_status ();
  (match listen with
  | `Tcp (ipaddr, port) ->
      Logs.info (fun m ->
          m "Listening on %s:%d" (Fmt.str "%a" Eio.Net.Ipaddr.pp ipaddr) port)
  | _ -> Logs.info (fun m -> m "Listening on <unknown>"));
  ()

let db_migrate () = Migration.migrate ()
let db_rollback () = Migration.rollback ()

let db_reset () =
  Lwt_eio.run_lwt @@ fun () ->
  Db.debug_drop_all_tables_in_db ();%lwt
  Migration.migrate ();

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

let db_generate_migration name =
  let (y, m, d), ((hh, mm, ss), _) = Ptime.(now () |> to_date_time) in
  let timestamp = Printf.sprintf "%04d%02d%02d_%02d%02d%02d" y m d hh mm ss in
  let item_in_list =
    Printf.sprintf "(%s, (module M%s_%s));\n" timestamp timestamp name
  in
  let file_name = Printf.sprintf "m%s_%s.ml" timestamp name in
  let file_path = Filename.(concat "lib" (concat "migrate" file_name)) in
  let list_file_path = Filename.(concat "lib" "migrations.inc") in

  (* Check if pwd is correct. FIXME: Relax this restriction. *)
  (match Unix.((stat list_file_path).st_kind) with
  | S_REG -> ()
  | _ -> failwith "Invalid pwd; lib/migrations.inc does not exist.");

  (* Write the item into the list *)
  let oc =
    open_out_gen [ Open_wronly; Open_append; Open_binary ] 0o000 list_file_path
  in
  Fun.protect
    (fun () -> Out_channel.output_string oc item_in_list)
    ~finally:(fun () -> close_out oc);

  (* Create (boilerplate) migration file *)
  let oc = open_out file_path in
  Fun.protect
    (fun () ->
      {raw|
open Sqlx.Migration.Helper

let change =
  assert false
  (*
create_table ~table_name:"preview_cards_statuses"
  ~schema:
    [ {|preview_card_id BIGINT NOT NULL|}; {|status_id BIGINT NOT NULL|} ]

add_column ~table_name:"preview_cards" ~name:"blurhash" ~spec:"TEXT"
  *)
|raw}
      |> String.trim
      |> Out_channel.output_string oc)
    ~finally:(fun () -> close_out oc);

  Logs.info (fun m -> m "Migration %s_%s created" timestamp name);

  ()

let oauth_generate_access_token username =
  Lwt_eio.run_lwt @@ fun () ->
  (* Generate a new OAuth application named "Web" *)
  let web =
    Oauth_helper.generate_application ~name:"Web"
      ~redirect_uri:"urn:ietf:wg:oauth:2.0:oob" ~scopes:"read write follow push"
  in
  let user =
    let a = Db.(e @@ Account.get_one ~username) in
    Db.(e @@ User.get_one ~account_id:a#id)
  in
  let access_token =
    Oauth_helper.generate_access_token ~scopes:"read write follow push"
      ~resource_owner_id:user#id ~app:web ()
  in
  Lwt_io.printf "%s\n%!" access_token#token;%lwt
  Lwt.return_unit

let hidden_input f =
  let open Unix in
  let attr = tcgetattr stdin in
  attr.c_echo <- false;
  tcsetattr stdin TCSAFLUSH attr;
  let res = f () in
  attr.c_echo <- true;
  tcsetattr stdin TCSAFLUSH attr;
  res

let user_register ?username ?display_name ?email ?password () =
  Lwt_eio.run_lwt @@ fun () ->
  let username =
    match username with
    | Some s -> s
    | None ->
        print_string "Username: ";
        read_line () |> String.trim
  in
  let display_name =
    match display_name with
    | Some s -> s
    | None ->
        print_string "Display name: ";
        read_line () |> String.trim
  in
  let email =
    match email with
    | Some s -> s
    | None ->
        print_string "Email: ";
        read_line () |> String.trim
  in
  let password =
    match password with
    | Some s -> s
    | None ->
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

let account_fetch env =
  let accts = Db.(e Account.all) in
  accts
  |> List.iteri @@ fun i acct ->
     Logs.info (fun m -> m "[%d/%d] %s" (i + 1) (List.length accts) acct#uri);
     match acct#domain with
     | None -> ()
     | Some domain -> (
         try
           Activity.fetch_person env (`DomainUser (domain, acct#username))
           |> Activity.model_account_of_person ~original:acct
           |> (fun a -> Db.(e (Account.save_one a)))
           |> ignore
         with e ->
           Logs.err (fun m ->
               m "Couldn't fetch person: %s" (Printexc.to_string e)))

let webpush_generate_vapid_key () =
  let priv_key, pub_key = Webpush.Vapid.generate_keys () in
  Printf.printf "vapid_private_key: \"%s\"\n" priv_key;
  Printf.printf "vapid_public_key: \"%s\"\n" pub_key;
  ()

let webpush_deliver env username message =
  match
    Db.(e @@ Account.(get_one ~preload:[ `user [] ] ~username ~domain:None))
  with
  | exception Sqlx.Error.NoRowFound -> failwith "username is not found"
  | a -> (
      match a#user with
      | None -> failwith "username is not local"
      | Some u -> Webpush_helper.deliver env ~user_id:u#id message)

let () =
  Eio_main.run @@ fun env ->
  Lwt_eio.with_event_loop ~clock:env#clock @@ fun _ ->
  (*
  (let file_name = Config.log_file_path () in
   if file_name = "" then Logs.(add_reporter (make_stderr_reporter ~l:Debug))
   else Logs.(add_reporter (make_file_reporter ~l:Debug ~file_name ())));
   *)
  Fmt.set_style_renderer Fmt.stdout `Ansi_tty;
  Logs.set_reporter (Logs_fmt.reporter ());
  Logs.set_level (Some Logs.Debug);

  Logs.info (fun m -> m "========== Waq booted ==========");
  Config.to_list ()
  |> List.iter (fun (k, v) -> Logs.debug (fun m -> m "Config %s = %s" k v));

  Crypto.initialize env @@ fun () ->
  Db.initialize ();

  (* Parse command-line arguments and call a proper handler *)
  let open Cmdliner in
  Cmd.(
    group
      ~default:Term.(const server $ const env)
      (info "waq")
      [
        v (info "db:reset") Term.(const db_reset $ const ());
        v (info "db:migrate") Term.(const db_migrate $ const ());
        v (info "db:rollback") Term.(const db_rollback $ const ());
        v
          (info "db:generate_migration")
          Term.(
            const db_generate_migration
            $ Arg.(required & pos 0 (some string) None & info ~docv:"NAME" []));
        v
          (info "oauth:generate_access_token")
          Term.(
            const oauth_generate_access_token
            $ Arg.(
                required & pos 0 (some string) None & info ~docv:"USERNAME" []));
        v (info "user:register")
          Term.(
            const (fun username display_name email password ->
                user_register ?username ?display_name ?email ?password ())
            $ Arg.(
                value
                & opt (some string) None
                & info ~docv:"USERNAME" [ "username" ])
            $ Arg.(
                value
                & opt (some string) None
                & info ~docv:"DISPLAY-NAME" [ "display-name" ])
            $ Arg.(
                value & opt (some string) None & info ~docv:"EMAIL" [ "email" ])
            $ Arg.(
                value
                & opt (some string) None
                & info ~docv:"PASSWORD" [ "password" ]));
        v (info "account:fetch") Term.(const account_fetch $ const env);
        v
          (info "webpush:generate_vapid_key")
          Term.(const webpush_generate_vapid_key $ const ());
        v (info "webpush:deliver")
          Term.(
            const webpush_deliver $ const env
            $ Arg.(
                required & pos 0 (some string) None & info ~docv:"USERNAME" [])
            $ Arg.(
                required & pos 1 (some string) None & info ~docv:"MESSAGE" []));
        v (info "server") Term.(const server $ const env);
      ])
  |> Cmd.eval |> exit
