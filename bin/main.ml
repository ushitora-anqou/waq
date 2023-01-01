open Waq
module C = Config

let () =
  Log.initialize Debug;
  C.load_file "config.yml";
  Http.Signature.initialize ();
  Db.initialize ();
  let host, port = (C.listen_host (), C.listen_port ()) in
  Router.routes
  |> Http.start_server ~host ~port @@ fun () ->
     Log.info (fun m -> m "Listening on %s:%d" host port);
     (try%lwt Db.rollback () with _ -> Lwt.return_unit);%lwt
     Db.migrate ();%lwt
     let now = Unix.gettimeofday () |> Ptime.of_float_s |> Option.get in
     let%lwt a =
       let username = "foobar" in
       let display_name = "Foobar's display name" in
       let created_at, updated_at = (now, now) in
       let private_key, public_key = Http.Signature.generate_keypair () in
       let public_key = Http.Signature.encode_public_key public_key in
       let private_key = Http.Signature.encode_private_key private_key in
       let uri = Router.url [ "users"; username ] in
       let inbox_url = Router.(uri ^/ "inbox") in
       Db.make_account ~username ~public_key ~private_key ~display_name ~uri
         ~inbox_url ~created_at ~updated_at ()
       |> Db.insert_account
     in
     let%lwt _u =
       let email = "foobar@example.com" in
       let created_at, updated_at = (now, now) in
       Db.make_user ~id:0 ~email ~created_at ~updated_at ~account_id:a.id
       |> Db.insert_user
     in
     let%lwt _ =
       Router.ToServer.fetch_account ~scheme:"http" "localhost:3000" "admin"
     in
     Lwt.return_unit
