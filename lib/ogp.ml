open Lwt.Infix
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
      ~provider_url
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

let fetch_oembed url =
  Throttle_fetch.http_get url
  >|= find_json_oembed_href >>= Throttle_fetch.http_get
  >|= parse_json_oembed ~url

let fetch_oembed_opt url =
  try%lwt fetch_oembed url >|= Option.some with _ -> Lwt.return_none

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

  make_oembed ~url ~typ:"link" ~title ~description ?image ()

let fetch_image_info url = Throttle_fetch.http_get url >>= Image.inspect

let fetch_opengraph url =
  let%lwt src = Throttle_fetch.http_get url >|= parse_opengraph ~url in
  match src.image with
  | None -> Lwt.return src
  | Some image_url ->
      let%lwt width, height, blurhash = fetch_image_info image_url in
      let blurhash = Some blurhash in
      Lwt.return { src with width; height; blurhash }

let fetch_opengraph_opt url =
  try%lwt fetch_opengraph url >|= Option.some
  with e ->
    Logq.err (fun m ->
        m "Couldn't fetch opengraph: %s\n%s" (Printexc.to_string e)
          (Printexc.get_backtrace ()));
    Lwt.return_none
