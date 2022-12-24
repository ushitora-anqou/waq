open Waq
module H = Handler

let () =
  Log.(add_reporter (make_reporter ~l:Debug ()));
  let open Http in
  let host, port = ("localhost", 8000) in
  router [ get "/.well-known/host-meta" H.well_known_host_meta ]
  |> Http.start_server ~host ~port (fun () ->
         Printf.eprintf "Listening on %s:%d\n%!" host port)
