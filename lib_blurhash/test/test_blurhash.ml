open Blurhash

let test_base83_case1 () =
  assert (encode_int ~value:0 ~length:2 = "00");
  assert (encode_int ~value:1 ~length:2 = "01");
  assert (encode_int ~value:83 ~length:2 = "10");
  assert (encode_int ~value:(83 * 83) ~length:2 = "00");
  ()

let blurhash_file ~x_components ~y_components path =
  let src =
    match OImages.(load path [] |> tag) with
    | Rgb24 img -> img
    | Rgba32 img -> img#to_rgb24
    | Index8 img -> img#to_rgb24
    | Index16 img -> img#to_rgb24
    | Cmyk32 _ -> failwith "Not supported image type: Cmyk32"
  in
  blur_hash_for_pixels ~x_components ~y_components ~width:src#width
    ~height:src#height ~rgb:(Bytes.to_string src#dump)
    ~bytes_per_row:(src#width * 3)

let test_blurhash_case1 () =
  let ic = open_in_bin "../../../../lib_blurhash/test/test.bin" in
  Fun.protect
    (fun () ->
      let rgb = In_channel.input_all ic in
      let hash =
        blur_hash_for_pixels ~x_components:4 ~y_components:3 ~width:204
          ~height:204 ~rgb ~bytes_per_row:(204 * 3)
      in
      assert (hash = "LFE.@D9F01_2%L%MIVD*9Goe-;WB");
      ())
    ~finally:(fun () -> close_in ic)

let test_blurhash_case2 () =
  assert (
    blurhash_file ~x_components:4 ~y_components:3
      "../../../../lib_blurhash/test/pic2.png"
    = "LlMF%n00%#MwS|WCWEM{R*bbWBbH")

let test_blurhash_case3 () =
  assert (
    blurhash_file ~x_components:4 ~y_components:3
      "../../../../lib_blurhash/test/pic2_bw.png"
    = "LjIY5?00?bIUofWBWBM{WBofWBj[")

let () =
  let open Alcotest in
  run "blurhash"
    [
      ("base83", [ test_case "case1" `Quick test_base83_case1 ]);
      ( "blurhash",
        [
          test_case "case1" `Quick test_blurhash_case1;
          test_case "case2" `Quick test_blurhash_case2;
          test_case "case3" `Quick test_blurhash_case3;
        ] );
    ]
