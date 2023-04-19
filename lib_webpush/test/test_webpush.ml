open Webpush

let test_main_case1 () =
  let endpoint = "https://updates.push.services.mozilla.com/wpush/v2/gAAAAA" in
  let subscriber = "mailto:example@example.com" in
  let vapid_priv_key = "IQ9Ur0ykXoHS9gzfYX0aBjy9lvdrjx_PFUXmie9YRcY" in
  let p256dh_key =
    "BNNL5ZaTfK81qhXOx23-wewhigUeFb632jN6LvRWCFH1ubQr77FE_9qV1FuojuRmHP42zmf34rXgW80OvUVDgTk"
  in
  let auth_key = "zqbxT6JKstKSY9JKibZLSQ" in
  let message = "Test" in
  match
    construct_request ~message ~auth_key ~p256dh_key ~subscriber ~endpoint
      ~vapid_priv_key
  with
  | Error _ -> assert false
  | Ok _ -> ()

let () =
  let open Alcotest in
  Mirage_crypto_rng_unix.initialize ();
  run "webpush" [ ("main", [ test_case "case1" `Quick test_main_case1 ]) ]
