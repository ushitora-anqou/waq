open Yume

let test_basic () =
  let recv_text = ref "" in
  let expected_string = "TEST TEXT" in
  Eio_main.run (fun env ->
      Mirage_crypto_rng_eio.run (module Mirage_crypto_rng.Fortuna) env
      @@ fun () ->
      try
        Eio.Time.with_timeout_exn env#clock 3.0 @@ fun () ->
        let handler =
          let open Server in
          default_handler
          |> Router.(
               use
                 [
                   get "/ws" (fun _ req ->
                       websocket req (fun ws_conn ->
                           recv_text :=
                             ws_recv ws_conn |> Option.value ~default:!recv_text));
                 ])
        in
        let listen =
          Eio.Net.getaddrinfo_stream ~service:"0" env#net "localhost" |> List.hd
        in
        Eio.Switch.run @@ fun sw ->
        Server.start_server env ~listen ~sw handler (fun socket ->
            let listening_port =
              match Eio.Net.listening_addr socket with
              | `Tcp (_, port) -> port
              | _ -> assert false
            in
            let ws_conn =
              let rec loop () =
                Eio.Time.sleep env#clock 1.0;
                try
                  Ws.Client.connect ~sw env
                    (Printf.sprintf "http://localhost:%d/ws" listening_port)
                with _ -> loop ()
              in
              loop ()
            in
            Ws.Client.write ws_conn
              (Websocket.Frame.create ~opcode:Text ~content:expected_string ());
            ())
      with Eio.Time.Timeout -> ());
  assert (!recv_text = expected_string);
  ()

let () =
  let open Alcotest in
  run "ws" [ ("basic", [ test_case "case1" `Quick test_basic ]) ]
