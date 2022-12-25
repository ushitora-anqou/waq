open Jingoo
open Jg_types
module C = Config

let ( ^/ ) s1 s2 = s1 ^ "/" ^ s2

let url (l : string list) =
  "https:/" ^ (C.server_name () :: l |> List.fold_left ( ^/ ) "")

let well_known_host_meta _req =
  let body =
    Jg_template.from_string
      ~models:[ ("url", Tstr (url [ ".well-known"; "webfinger" ])) ]
      {|<?xml version="1.0" encoding="UTF-8"?>
<XRD xmlns="http://docs.oasis-open.org/ns/xri/xrd-1.0">
  <Link rel="lrdd" template="{{ url }}?resource={uri}"/>
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
        let link = url [ "users"; name ] in
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

type get_users_res_public_key = {
  id : string;
  owner : string;
  publicKeyPem : string;
}
[@@deriving make, yojson { strict = false }]

type get_users_res = {
  context : string; [@key "@context"]
  id : string;
  typ : string; [@key "type"]
  following : string;
  followers : string;
  inbox : string;
  outbox : string;
  preferredUsername : string;
  name : string;
  summary : string;
  url : string;
  tag : string list; (*publicKey : get_users_res_public_key;*)
}
[@@deriving make, yojson { strict = false }]

let get_users req =
  let username = Http.param ":name" req in
  let link = url [ "users"; username ] in
  let _key =
    make_get_users_res_public_key ~id:(link ^ "#main-key") ~owner:link
      ~publicKeyPem:"FIXME"
  in
  let body =
    make_get_users_res ~context:"https://www.w3.org/ns/activitystreams" ~id:link
      ~typ:"Person" ~following:(link ^/ "following")
      ~followers:(link ^/ "followers") ~inbox:(link ^/ "inbox")
      ~outbox:(link ^/ "outbox") ~preferredUsername:username
      ~name:"Name is here" ~summary:"Summary is here" ~url:link ~tag:[]
      (*~publicKey:key*) ()
    |> get_users_res_to_yojson |> Yojson.Safe.to_string
  in
  Http.respond
    ~headers:[ ("Content-Type", "application/jrd+json; charset=utf-8") ]
    body

type post_inbox_req = {
  context : Yojson.Safe.t; [@key "@context"]
  id : string;
  typ : string; [@key "type"]
  actor : Yojson.Safe.t;
  obj : Yojson.Safe.t; [@key "object"]
}
[@@deriving yojson { strict = false }]

let post_inbox req =
  let open Lwt.Syntax in
  let* body = Http.body req in
  let j = Yojson.Safe.from_string body in
  match post_inbox_req_of_yojson j with
  | Error _ ->
      Log.debug (fun m -> m "Error:\n%s" (Yojson.Safe.pretty_to_string j));
      Http.respond ~status:`Bad_request ""
  | Ok r ->
      Log.debug (fun m ->
          m "Activity: %s\n%s" r.typ (Yojson.Safe.pretty_to_string j));
      Http.respond ~status:`OK ""

let routes =
  let open Http in
  router
    [
      get "/.well-known/host-meta" well_known_host_meta;
      get "/.well-known/webfinger" well_known_webfinger;
      get "/users/:name" get_users;
      post "/users/:name/inbox" post_inbox;
    ]
