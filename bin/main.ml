open Waq
module H = Handler

let () =
  Log.(add_reporter (make_reporter ~l:Debug ()));
  let open Http in
  let host, port = ("localhost", 8000) in
  router
    [
      get "/.well-known/host-meta" H.well_known_host_meta;
      get "/.well-known/webfinger" H.well_known_webfinger;
    ]
  |> Http.start_server ~host ~port @@ fun () ->
     Log.info (fun m -> m "Listening on %s:%d" host port)
