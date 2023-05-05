open Blurhash

let test_base83_case1 () =
  assert (encode_int ~value:0 ~length:2 = "00");
  assert (encode_int ~value:1 ~length:2 = "01");
  assert (encode_int ~value:83 ~length:2 = "10");
  assert (encode_int ~value:(83 * 83) ~length:2 = "00");
  ()

let () =
  let open Alcotest in
  run "blurhash" [ ("base83", [ test_case "case1" `Quick test_base83_case1 ]) ]
