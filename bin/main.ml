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
     let%lwt _ = Db.rollback () in
     match%lwt Db.migrate () with
     | Ok () -> Lwt.return_unit
     | Error e ->
         Log.err (fun m -> m "Migration failed: %s" e);
         Lwt.fail_with "Migration failed"
