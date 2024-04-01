open Util

type oembed = {
  url : string;
  title : string;
  description : string; [@default ""]
  typ : string; [@key "type"]
  author_name : string; [@default ""]
  author_url : string; [@default ""]
  provider_name : string; [@default ""]
  provider_url : string; [@default ""]
  html : string; [@default ""]
  width : int; [@default 0]
  height : int; [@default 0]
  image : string option; [@yojson.option]
  embed_url : string; [@default ""]
  blurhash : string option; [@yojson.option]
}
[@@deriving make, yojson]

let expect_assoc = function
  | `Assoc x -> x
  | _ -> failwith "Expect `Assoc but got something else"

let expect_string = function
  | `String x -> x
  | _ -> failwith "Expect `String but got something else"

let expect_int = function
  | `Int x -> x
  | _ -> failwith "Expect `Int but got something else"

let find_json_oembed_href src =
  let open Soup in
  let soup = parse src in
  match
    soup $? "link[type='application/json+oembed']"
    |> Option.map (R.attribute "href")
  with
  | Some x -> x
  | None -> failwith "No oembed href"

let parse_json_oembed ~url src =
  let oembed = src |> Yojson.Safe.from_string |> expect_assoc in

  let expect name = List.assoc name oembed in
  let string name = expect name |> expect_string in
  let int name = expect name |> expect_int in

  let typ = string "type" in
  let title = string "title" in
  let author_name = string "author_name" in
  let author_url = string "author_url" in
  let provider_name = string "provider_name" in
  let provider_url = string "provider_url" in

  let make =
    make_oembed ~url ~typ ~title ~author_name ~author_url ~provider_name
      ~provider_url ~blurhash:Image.dummy_blurhash
  in
  match typ with
  | "video" ->
      let width = int "width" in
      let height = int "height" in
      let html = string "html" (* FIXME: Sanitize *) in
      let image = string "thumbnail_url" in
      make ~width ~height ~html ~image ()
  | "photo" ->
      let width = int "width" in
      let height = int "height" in
      let image = string "url" in
      let embed_url = string "url" in
      make ~width ~height ~embed_url ~image ()
  | "link" ->
      let image = string "thumnail_url" in
      make ~image ()
  | _ -> failwith "Invalid type"

let fetch_oembed env url =
  Throttle_fetch.http_get env url
  |> find_json_oembed_href
  |> Throttle_fetch.http_get env
  |> parse_json_oembed ~url

let fetch_oembed_opt env url = try Some (fetch_oembed env url) with _ -> None

let parse_opengraph ~url src =
  let open Soup in
  let soup = parse src in

  let opengraph_tag name =
    let x1 = soup $? "meta[property=\"" ^ name ^ "\"]" in
    let x2 = soup $? "meta[name=\"" ^ name ^ "\"]" in
    match (x1, x2) with
    | Some x, _ | None, Some x -> Some (R.attribute "content" x)
    | _ -> None
  in
  let meta_tag name =
    Option.bind (soup $? "meta[name=\"" ^ name ^ "\"]") (attribute "content")
  in

  let title =
    [
      opengraph_tag "og:title";
      soup $? "title" |> Option.map R.leaf_text;
      Some "";
    ]
    |> List.find Option.is_some |> Option.get
  in
  let description =
    [ opengraph_tag "og:description"; meta_tag "description"; Some "" ]
    |> List.find Option.is_some |> Option.get
  in
  let image =
    opengraph_tag "og:image"
    |> Option.map
         Uri.(of_string *> resolve "https" (of_string url) *> to_string)
  in
  let provider_name = opengraph_tag "og:site_name" in

  make_oembed ~url ~typ:"link" ~title ~description ?image ?provider_name ()

let fetch_image_info env url =
  let body = Throttle_fetch.http_get env url in
  Lwt_eio.run_lwt @@ fun () -> Image.inspect body

let fetch_opengraph env url =
  let src = Throttle_fetch.http_get env url |> parse_opengraph ~url in
  match src.image with
  | None -> src
  | Some image_url -> (
      try
        let width, height, blurhash = fetch_image_info env image_url in
        let blurhash = Some blurhash in
        { src with width; height; blurhash }
      with _ -> { src with image = None })

let fetch_opengraph_opt env url =
  try Some (fetch_opengraph env url)
  with e ->
    Logs.err (fun m ->
        m "Couldn't fetch opengraph: %s\n%s" (Printexc.to_string e)
          (Printexc.get_backtrace ()));
    None
