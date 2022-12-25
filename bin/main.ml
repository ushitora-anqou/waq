open Waq
module C = Config
module H = Handler

let () =
  Log.(add_reporter (make_reporter ~l:Debug ()));
  let open Http in
  C.load_file "config.yml";
  let host, port = (C.listen_host (), C.listen_port ()) in
  router
    [
      get "/.well-known/host-meta" H.well_known_host_meta;
      get "/.well-known/webfinger" H.well_known_webfinger;
    ]
  |> Http.start_server ~host ~port @@ fun () ->
     Log.info (fun m -> m "Listening on %s:%d" host port)
