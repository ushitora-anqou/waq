open Jingoo
open Jg_types
module C = Config

let well_known_host_meta _req =
  let body =
    Jg_template.from_string
      ~models:[ ("server_name", Tstr (C.server_name ())) ]
      {|<?xml version="1.0" encoding="UTF-8"?>
<XRD xmlns="http://docs.oasis-open.org/ns/xri/xrd-1.0">
  <Link rel="lrdd" template="{{ server_name }}/.well-known/webfinger?resource={uri}"/>
</XRD>
|}
  in
  Http.respond
    ~headers:[ ("Content-Type", "application/xrd+xml; charset=utf-8") ]
    body

type webfinger_res_link = {
  rel : string;
  typ : string; [@key "type"]
  href : string;
}
[@@deriving make, yojson { strict = false }]

type webfinger_res = {
  subject : string;
  aliases : string list;
  links : webfinger_res_link list;
}
[@@deriving make, yojson { strict = false }]

let well_known_webfinger req =
  let server_name = C.server_name () in
  match req |> Http.query_opt "resource" with
  | Some [ s ] ->
      let s =
        (* Remove 'acct:' prefix if exists *)
        if String.starts_with ~prefix:"acct:" s then
          String.sub s 5 (String.length s - 5)
        else s
      in
      (* Get account name and domain and check if they are correct
         FIXME: Check if the account exists *)
      let s = String.split_on_char '@' s in
      if not (List.length s = 2 && C.is_my_domain (List.nth s 1)) then
        Http.respond ~status:`Not_found ""
      else
        (* Return the body *)
        let name, dom = (List.hd s, List.nth s 1) in
        let link = server_name ^ "/users/" ^ name in
        let body =
          make_webfinger_res
            ~subject:("acct:" ^ name ^ "@" ^ dom)
            ~aliases:[ link ]
            ~links:
              [
                make_webfinger_res_link ~rel:"self"
                  ~typ:"application/activity+json" ~href:link;
              ]
            ()
          |> webfinger_res_to_yojson |> Yojson.Safe.to_string
        in
        Http.respond
          ~headers:[ ("Content-Type", "application/jrd+json; charset=utf-8") ]
          body
  | _ -> Http.respond ~status:`Not_found ""

let routes =
  let open Http in
  router
    [
      get "/.well-known/host-meta" well_known_host_meta;
      get "/.well-known/webfinger" well_known_webfinger;
    ]
