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
  Throttle_fetch.f_exn url >|= find_json_oembed_href >>= Throttle_fetch.f_exn
  >|= parse_json_oembed ~url
