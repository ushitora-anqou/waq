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
