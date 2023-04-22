open Waq

let read_file filename =
  let ic = open_in filename in
  Fun.protect
    ~finally:(fun () -> close_in ic)
    (fun () -> In_channel.input_all ic)

let test_oembed_flickr () =
  let src = read_file "../../../test/test_ogp_flickr.html" in

  assert (
    Ogp.find_json_oembed_href src
    = "https://www.flickr.com/services/oembed?url=https://www.flickr.com/photos/tomfenskephotography/49088768431&format=json");

  let src = read_file "../../../test/test_ogp_flickr.json" in
  let url = "https://www.flickr.com/photos/tomfenskephotography/49088768431/" in
  let x = Ogp.parse_json_oembed ~url src in
  assert (x.url = url);
  assert (x.title = "Oregon");
  assert (x.description = "");
  assert (x.typ = "photo");
  assert (x.author_name = "Tom Fenske Photography");
  assert (x.author_url = "https://www.flickr.com/photos/tomfenskephotography/");
  assert (x.provider_name = "Flickr");
  assert (x.provider_url = "https://www.flickr.com/");
  assert (x.html = "");
  assert (x.width = 1024);
  assert (x.height = 427);
  assert (
    x.image
    = Some "https://live.staticflickr.com/65535/49088768431_6a4322b3bb_b.jpg");
  assert (
    x.embed_url
    = "https://live.staticflickr.com/65535/49088768431_6a4322b3bb_b.jpg");

  (*assert (x.blurhash = None); (* FIXME *)*)
  ()

let test_oembed_youtube () =
  let src = read_file "../../../test/test_ogp_youtube.html" in
  assert (
    Ogp.find_json_oembed_href src
    = "https://www.youtube.com/oembed?format=json&url=https%3A%2F%2Fwww.youtube.com%2Fwatch%3Fv%3DOMv_EPMED8Y");

  let src = read_file "../../../test/test_ogp_youtube.json" in
  let url = "https://www.youtube.com/watch?v=OMv_EPMED8Y" in
  let x = Ogp.parse_json_oembed ~url src in
  assert (x.url = url);
  assert (x.title = "♪ Brand New Friend (Christmas Song!)");
  assert (x.description = "");
  assert (x.typ = "video");
  assert (x.author_name = "YOGSCAST Lewis & Simon");
  assert (x.author_url = "https://www.youtube.com/@yogscast");
  assert (x.provider_name = "YouTube");
  assert (x.provider_url = "https://www.youtube.com/");
  assert (x.html <> "" (* FIXME *));
  assert (x.width = 200);
  assert (x.height = 113);
  assert (Option.is_some x.image);
  assert (x.embed_url = "");

  (*assert (x.blurhash = None); (* FIXME *)*)
  ()

let () =
  let open Alcotest in
  run "ogp"
    [
      ( "oembed",
        [
          test_case "flickr" `Quick test_oembed_flickr;
          test_case "youtube" `Quick test_oembed_youtube;
        ] );
    ]
