(* Recv GET /.well-known/host-meta *)
let get _req =
  let url = Config.url [ ".well-known"; "webfinger" ] in
  Jingoo.Jg_template.from_string
    ~models:[ ("url", Tstr url) ]
    {|<?xml version="1.0" encoding="UTF-8"?>
<XRD xmlns="http://docs.oasis-open.org/ns/xri/xrd-1.0">
  <Link rel="lrdd" template="{{ url }}?resource={uri}"/>
</XRD>|}
  |> Httpq.Server.respond ~headers:[ Helper.content_type_app_xrd_xml ]
