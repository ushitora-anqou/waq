open Waq
module C = Config

let () =
  Log.(add_reporter (make_reporter ~l:Debug ()));
  C.load_file "config.yml";
  let host, port = (C.listen_host (), C.listen_port ()) in
  Router.routes
  |> Http.start_server ~host ~port @@ fun () ->
     Log.info (fun m -> m "Listening on %s:%d" host port)
