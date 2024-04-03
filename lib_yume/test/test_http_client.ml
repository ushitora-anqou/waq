let test_get () =
  Eio_main.run @@ fun env ->
  Mirage_crypto_rng_eio.run (module Mirage_crypto_rng.Fortuna) env @@ fun () ->
  [
    "https://test-ev-rsa.ssl.com/";
    "https://test-dv-rsa.ssl.com/";
    "https://test-ev-ecc.ssl.com/";
    "https://test-dv-ecc.ssl.com/";
  ]
  |> List.map (fun url () ->
         Yume.Client.fetch env url |> Result.get_ok |> ignore)
  |> Eio.Fiber.all;

  [
    "https://expired-rsa-dv.ssl.com/";
    "https://expired-rsa-ev.ssl.com/";
    "https://expired-ecc-dv.ssl.com/";
    "https://expired-ecc-ev.ssl.com/";
  ]
  |> List.map (fun url () ->
         Yume.Client.fetch env url |> Result.get_error |> ignore)
  |> Eio.Fiber.all;

  (*
     FIXME: Do OCSP stuff
  ([
     "https://revoked-rsa-dv.ssl.com/";
     "https://revoked-rsa-ev.ssl.com/";
     "https://revoked-ecc-dv.ssl.com/";
     "https://revoked-ecc-ev.ssl.com/";
   ]
  |> List.iter @@ fun url ->
     Yume.Client.fetch env url |> Result.get_error |> ignore);
     *)
  ()

let () =
  let open Alcotest in
  Fmt.set_style_renderer Fmt.stdout `Ansi_tty;
  Logs.set_reporter (Logs_fmt.reporter ());
  Logs.set_level (Some Logs.Debug);
  run "http client" [ ("get", [ test_case "case1" `Quick test_get ]) ]
