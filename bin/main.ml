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
     try%lwt Db.migrate () with _ -> Lwt.fail_with "Migration failed"
