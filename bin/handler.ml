open Waq
open Jingoo
open Jg_types

let schema = "http"
let domain_name = "localhost:8080"

let well_known_host_meta _req =
  let body =
    Jg_template.from_string
      ~models:[ ("schema", Tstr schema); ("domain", Tstr domain_name) ]
      {|<?xml version="1.0" encoding="UTF-8"?>
<XRD xmlns="http://docs.oasis-open.org/ns/xri/xrd-1.0">
  <Link rel="lrdd" template="{{ schema }}://{{ domain }}/.well-known/webfinger?resource={uri}"/>
</XRD>
|}
  in
  Http.respond
    ~headers:[ ("Content-Type", "application/xrd+xml; charset=utf-8") ]
    body

let well_known_webfinger req =
  match req |> Http.query_opt "resource" with
  | Some [ "acct:anqou@localhost:8000" ] ->
      let body =
        String.trim
          {|
{
  "subject": "acct:anqou@localhost:8000",
  "aliases": [
    "https://localhost:8000/users/anqou"
  ],
  "links": [
    {
      "rel": "self",
      "type": "application/activity+json",
      "href": "https://localhost:8000/users/anqou"
    }
  ]
}
|}
      in
      Http.respond
        ~headers:[ ("Content-Type", "application/jrd+json; charset=utf-8") ]
        body
  | _ -> Http.respond ~status:`Bad_request ""
