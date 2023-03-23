open Activity
open Helper
open Util

let respond_jrd w () =
  w |> yojson_of_webfinger |> Yojson.Safe.to_string
  |> Httpq.Server.respond ~headers:[ Helper.content_type_app_jrd_json ]

let respond_xrd w () =
  ({|<?xml version="1.0" encoding="UTF-8"?>|}
  ^ Xml.(
      Element
        ( "XRD",
          [ ("xmlns", "http://docs.oasis-open.org/ns/xri/xrd-1.0") ],
          [ Element ("Subject", [], [ PCData w.subject ]) ]
          @ (w.aliases
            |> List.map (fun alias -> Element ("Alias", [], [ PCData alias ])))
          @ (w.links
            |> List.map (fun link ->
                   let link = webfinger_link_of_yojson link in
                   Element
                     ( "Link",
                       [
                         ("href", link.href);
                         ("rel", link.rel);
                         ("type", link.typ);
                       ],
                       [] ))) )
      |> to_string))
  |> Httpq.Server.respond ~headers:[ Helper.content_type_app_xrd_xml ]

(* Recv GET /.well-known/webfinger *)
let get req =
  try%lwt
    let s = req |> Httpq.Server.query "resource" in
    let s =
      (* Remove 'acct:' prefix if exists *)
      if String.starts_with ~prefix:"acct:" s then
        String.sub s 5 (String.length s - 5)
      else s
    in
    (* Get account name and domain and check if they are correct *)
    let s = String.split_on_char '@' s in
    if not (List.length s = 2 && Config.is_my_domain (List.nth s 1)) then
      failwith "Invalid request";
    (* Return the body *)
    let name, dom = (List.hd s, List.nth s 1) in
    let%lwt a = Db.(e & Account.get_one ~domain:None ~username:name) in
    let%lwt _ = Db.(e & User.get_one ~account_id:a#id) in
    let res =
      make_webfinger
        ~subject:("acct:" ^ name ^ "@" ^ dom)
        ~aliases:[ a#uri ]
        ~links:
          [
            make_webfinger_link ~rel:"self" ~typ:"application/activity+json"
              ~href:a#uri
            |> yojson_of_webfinger_link;
          ]
        ()
    in
    req
    |> render ~default:(respond_jrd res)
         [ ("application/xrd+xml", respond_xrd res) ]
  with e ->
    Logq.debug (fun m ->
        m "[well_known_webfinger] Can't find user: %s\n%s"
          (Printexc.to_string e)
          (Printexc.get_backtrace ()));
    Httpq.Server.raise_error_response `Not_found
