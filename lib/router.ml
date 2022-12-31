open Jingoo
open Jg_types
module C = Config

(* FIXME: Don't fix keypair *)
let private_key, public_key =
  Http.Signature.initialize ();
  Http.Signature.generate_keypair ()

let ( ^/ ) s1 s2 = s1 ^ "/" ^ s2

let url (l : string list) =
  "https:/" ^ (C.server_name () :: l |> List.fold_left ( ^/ ) "")

let unpack req f g =
  let%lwt body = Http.body req in
  Log.debug (fun m -> m "Body:\n%s\n%!" body);
  let j = Yojson.Safe.from_string body in
  match f j with
  | Error _ ->
      Log.debug (fun m -> m "Error:\n%s" (Yojson.Safe.pretty_to_string j));
      Http.respond ~status:`Bad_request ""
  | Ok r -> g j r

module FromServer = struct
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
    try%lwt
      let s = req |> Http.query "resource" |> List.hd in
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
        raise (failwith "Invalid request");
      (* Return the body *)
      let name, dom = (List.hd s, List.nth s 1) in
      let%lwt _ = Db.get_user_by_username name in
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
    with e ->
      Log.debug (fun m ->
          m "[well_known_webfinger] Can't find user: %s\n%s"
            (Printexc.to_string e)
            (Printexc.get_backtrace ()));
      Http.respond ~status:`Not_found ""

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
    tag : string list;
    publicKey : get_users_res_public_key;
  }
  [@@deriving make, yojson { strict = false }]

  let get_users req =
    (* FIXME: Check if the account exists *)
    let username = Http.param ":name" req in
    let link = url [ "users"; username ] in
    let publicKey =
      make_get_users_res_public_key ~id:(link ^ "#main-key") ~owner:link
        ~publicKeyPem:(Http.Signature.encode_public_key public_key)
    in
    let body =
      make_get_users_res ~context:"https://www.w3.org/ns/activitystreams"
        ~id:link ~typ:"Person" ~following:(link ^/ "following")
        ~followers:(link ^/ "followers") ~inbox:(link ^/ "inbox")
        ~outbox:(link ^/ "outbox") ~preferredUsername:username
        ~name:"Name is here" ~summary:"Summary is here" ~url:link ~tag:[]
        ~publicKey ()
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
    unpack req post_inbox_req_of_yojson @@ fun j r ->
    Log.debug (fun m ->
        m "Activity: %s\n%s\n%s" r.typ
          (Http.headers req
          |> List.map (fun (k, v) -> k ^ ": " ^ v)
          |> String.concat "\n")
          (Yojson.Safe.pretty_to_string j));
    Http.respond ~status:`OK ""
end

module ToServer = struct
  type post_inbox_req = {
    context : Yojson.Safe.t; [@key "@context"]
    id : string;
    typ : string; [@key "type"]
    actor : Yojson.Safe.t;
    obj : Yojson.Safe.t; [@key "object"]
  }
  [@@deriving make, yojson { strict = false }]

  let post_users_inbox_follow _id =
    let link = url [ "users"; "anqou" ] in
    let body =
      make_post_inbox_req
        ~context:(`String "https://www.w3.org/ns/activitystreams")
        ~id:(link ^ "#follow/1") ~typ:"Follow" ~actor:(`String link)
        ~obj:(`String "http://localhost:3000/users/admin")
      |> post_inbox_req_to_yojson |> Yojson.Safe.to_string
    in
    let sign =
      let priv_key = private_key in
      let key_id = link ^ "#main-key" in
      let signed_headers =
        [ "(request-target)"; "host"; "date"; "digest"; "content-type" ]
      in
      Some (priv_key, key_id, signed_headers)
    in
    let meth = `POST in
    let headers = [ ("Content-Type", "application/activity+json") ] in
    let%lwt res =
      Http.fetch ~meth ~headers ~body ~sign
        "http://localhost:3000/users/admin/inbox"
    in
    Lwt.return
    @@
    match res with
    | Ok (status, _body) when Httpaf.Status.is_successful status -> Ok ()
    | _ -> Error ()
end

module ToClient = struct
  (*
  type post_api_v1_statuses_req = { status : string }
  [@@deriving make, yojson { strict = false }]

  type post_api_v1_statuses_res = {
    id : string;
    created_at : string;
    content : string;
  }
  [@@deriving make, yojson { strict = false }]

  let post_api_v1_statuses _req =
    unpack req post_api_v1_statuses_req_of_yojson @@ fun _ r ->
    Log.debug (fun m -> m "Post \"%s\"" r.status);
    Http.respond ~status:`OK ""
    *)

  type post_api_v1_accounts_follow_res = {
    id : string;
    following : bool;
    showing_reblogs : bool;
    notifying : bool;
    followed_by : bool;
    blocking : bool;
    blocked_by : bool;
    muting : bool;
    muting_notifications : bool;
    requested : bool;
    domain_blocking : bool;
    endorsed : bool;
  }
  [@@deriving make, yojson { strict = false }]

  let post_api_v1_accounts_follow req =
    let id = Http.param ":id" req in
    let%lwt res = ToServer.post_users_inbox_follow id in
    match res with
    | Ok _ ->
        make_post_api_v1_accounts_follow_res ~id ~following:true
          ~showing_reblogs:true ~notifying:false ~followed_by:false
          ~blocking:false ~blocked_by:false ~muting:false
          ~muting_notifications:false ~requested:false ~domain_blocking:false
          ~endorsed:false
        |> post_api_v1_accounts_follow_res_to_yojson |> Yojson.Safe.to_string
        |> Http.respond ~status:`OK
    | _ -> Http.respond ~status:`Internal_server_error ""
end

let routes =
  let open Http in
  router
    [
      (* to servers *)
      get "/.well-known/host-meta" FromServer.well_known_host_meta;
      get "/.well-known/webfinger" FromServer.well_known_webfinger;
      get "/users/:name" FromServer.get_users;
      post "/users/:name/inbox" FromServer.post_inbox;
      (* to clients *)
      (*
      post "/api/v1/statuses" ToClient.post_api_v1_statuses;
      *)
      post "/api/v1/accounts/:id/follow" ToClient.post_api_v1_accounts_follow;
    ]
