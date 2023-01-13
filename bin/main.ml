open Waq
open Util
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

     Log.debug (fun m -> m "Connect to PostgreSQL");
     let%lwt _ =
       let open Db_ in
       let%lwt c = connect (Config.db_url ()) in
       execute c "CREATE TABLE foobar ( id SERIAL PRIMARY KEY )";%lwt
       execute c "DROP TABLE foobar"
     in

     (try%lwt Db.rollback () with _ -> Lwt.return_unit);%lwt
     Db.migrate ();%lwt
     let now = Ptime.now () in
     let%lwt a =
       let username = "foobar" in
       let display_name = "Foobar's display name" in
       let created_at, updated_at = (now, now) in
       let private_key, public_key = Http.Signature.generate_keypair () in
       let public_key = Http.Signature.encode_public_key public_key in
       let private_key = Http.Signature.encode_private_key private_key in
       let uri = Activity.url [ "users"; username ] in
       let inbox_url = Activity.(uri ^/ "inbox") in
       let followers_url = Activity.(uri ^/ "followers") in
       Db.make_account ~username ~public_key ~private_key ~display_name ~uri
         ~inbox_url ~followers_url ~created_at ~updated_at ()
       |> Db.upsert_account
     in
     assert (a.id = 1);
     let%lwt _u =
       let email = "foobar@example.com" in
       let created_at, updated_at = (now, now) in
       Db.make_user ~id:0 ~email ~created_at ~updated_at ~account_id:a.id
       |> Db.insert_user
     in
     Lwt.return_unit
