module C = Config

let ( ^/ ) s1 s2 = s1 ^ "/" ^ s2

let url (l : string list) =
  "https:/" ^ (C.server_name () :: l |> List.fold_left ( ^/ ) "")

(* .well-known/webfinger *)
type webfinger_link = {
  rel : string;
  typ : string; [@key "type"]
  href : string;
}
[@@deriving make, yojson { strict = false }]

type webfinger = {
  subject : string;
  aliases : string list;
  links : Yojson.Safe.t list;
}
[@@deriving make, yojson { strict = false }]

(* /users/:name *)
type ap_user_public_key = { id : string; owner : string; publicKeyPem : string }
[@@deriving make, yojson { strict = false }]

type ap_user = {
  context : Yojson.Safe.t; [@key "@context"]
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
  publicKey : ap_user_public_key;
}
[@@deriving make, yojson { strict = false }]

(* /users/:name/inbox *)
type ap_inbox = {
  context : Yojson.Safe.t; [@key "@context"]
  id : string;
  typ : string; [@key "type"]
  actor : Yojson.Safe.t;
  obj : Yojson.Safe.t; [@key "object"]
}
[@@deriving make, yojson { strict = false }]

module ToServer = struct
  (* Send GET /.well-known/webfinger and GET /users/:name *)
  let fetch_account ?(scheme = "https") domain username =
    let now = Unix.gettimeofday () |> Ptime.of_float_s |> Option.get in

    (* FIXME: Check /.well-known/host-meta if necessary *)
    let%lwt body =
      Http.fetch_exn @@ scheme ^ ":/" ^/ domain
      ^/ ".well-known/webfinger?resource=acct:" ^ username ^ "@" ^ domain
    in
    let webfinger =
      body |> Yojson.Safe.from_string |> webfinger_of_yojson |> Result.get_ok
    in
    let href =
      webfinger.links
      |> List.find_map (fun l ->
             match webfinger_link_of_yojson l with
             | Ok l when l.rel = "self" -> Some l.href
             | _ -> None)
      |> Option.get
    in

    let%lwt body =
      Http.fetch_exn ~headers:[ ("Accept", "application/activity+json") ] href
    in
    let r =
      body |> Yojson.Safe.from_string |> ap_user_of_yojson |> Result.get_ok
    in
    Db.make_account ~username:r.preferredUsername ~domain
      ~public_key:r.publicKey.publicKeyPem ~display_name:r.name ~uri:r.id
      ~url:r.url ~inbox_url:r.inbox ~created_at:now ~updated_at:now ()
    |> Db.insert_account

  (* Send Follow to POST /users/:name/inbox *)
  let post_users_inbox_follow self_id id =
    let%lwt self = Db.get_account ~id:self_id in
    let link = url [ "users"; self.username ] in
    let%lwt acc = Db.get_account ~id in
    let body =
      make_ap_inbox ~context:(`String "https://www.w3.org/ns/activitystreams")
        ~id:(link ^ "#follow/1") ~typ:"Follow" ~actor:(`String link)
        ~obj:(`String acc.uri)
      |> ap_inbox_to_yojson |> Yojson.Safe.to_string
    in
    let sign =
      let priv_key =
        self.private_key |> Option.get |> Http.Signature.decode_private_key
      in
      let key_id = link ^ "#main-key" in
      let signed_headers =
        [ "(request-target)"; "host"; "date"; "digest"; "content-type" ]
      in
      Some (priv_key, key_id, signed_headers)
    in
    let meth = `POST in
    let headers = [ ("Content-Type", "application/activity+json") ] in
    let%lwt res = Http.fetch ~meth ~headers ~body ~sign acc.inbox_url in
    Lwt.return
    @@
    match res with
    | Ok (status, _body) when Httpaf.Status.is_successful status -> Ok ()
    | _ -> Error ()
end

module FromServer = struct
  (* Recv GET /.well-known/host-meta *)
  let get_well_known_host_meta () =
    let url = url [ ".well-known"; "webfinger" ] in
    Jingoo.Jg_template.from_string
      ~models:[ ("url", Tstr url) ]
      {|<?xml version="1.0" encoding="UTF-8"?>
<XRD xmlns="http://docs.oasis-open.org/ns/xri/xrd-1.0">
  <Link rel="lrdd" template="{{ url }}?resource={uri}"/>
</XRD>|}
    |> Lwt.return

  (* Recv GET /.well-known/webfinger *)
  let get_well_known_webfinger s =
    try%lwt
      let s =
        (* Remove 'acct:' prefix if exists *)
        if String.starts_with ~prefix:"acct:" s then
          String.sub s 5 (String.length s - 5)
        else s
      in
      (* Get account name and domain and check if they are correct *)
      let s = String.split_on_char '@' s in
      if not (List.length s = 2 && C.is_my_domain (List.nth s 1)) then
        raise (failwith "Invalid request");
      (* Return the body *)
      let name, dom = (List.hd s, List.nth s 1) in
      let%lwt _ = Db.get_user_by_username name in
      let link = url [ "users"; name ] in
      make_webfinger
        ~subject:("acct:" ^ name ^ "@" ^ dom)
        ~aliases:[ link ]
        ~links:
          [
            make_webfinger_link ~rel:"self" ~typ:"application/activity+json"
              ~href:link
            |> webfinger_link_to_yojson;
          ]
        ()
      |> webfinger_to_yojson |> Yojson.Safe.to_string |> Result.ok |> Lwt.return
    with e ->
      Log.debug (fun m ->
          m "[well_known_webfinger] Can't find user: %s\n%s"
            (Printexc.to_string e)
            (Printexc.get_backtrace ()));
      Lwt.return (Error `Not_found)

  (* Recv GET /users/:name *)
  let get_users username =
    try%lwt
      let%lwt u = Db.get_user_by_username username in
      let%lwt a = Db.get_account ~id:u.account_id in
      let link = url [ "users"; username ] in
      let publicKey =
        make_ap_user_public_key ~id:(link ^ "#main-key") ~owner:link
          ~publicKeyPem:a.public_key
      in
      make_ap_user ~context:(`String "https://www.w3.org/ns/activitystreams")
        ~id:link ~typ:"Person" ~following:(link ^/ "following")
        ~followers:(link ^/ "followers") ~inbox:a.inbox_url
        ~outbox:(link ^/ "outbox") ~preferredUsername:username
        ~name:a.display_name ~summary:"Summary is here" ~url:link ~tag:[]
        ~publicKey ()
      |> ap_user_to_yojson |> Yojson.Safe.to_string |> Result.ok |> Lwt.return
    with e ->
      Log.debug (fun m ->
          m "[get_users] Can't find user: %s: %s\n%s" username
            (Printexc.to_string e)
            (Printexc.get_backtrace ()));
      Lwt.return (Error `Not_found)

  (* Recv POST /users/:name/inbox *)
  let post_users_inbox body () =
    match Yojson.Safe.from_string body |> ap_inbox_of_yojson with
    | Error _ -> Lwt.return_unit
    | Ok _r -> Lwt.return_unit
end

module FromClient = struct
  (* Recv POST /api/v1/accounts/:id/follow *)
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

  let post_api_v1_accounts_follow self_id id =
    let%lwt res = ToServer.post_users_inbox_follow self_id id in
    match res with
    | Ok _ ->
        make_post_api_v1_accounts_follow_res ~id:(string_of_int id)
          ~following:true ~showing_reblogs:true ~notifying:false
          ~followed_by:false ~blocking:false ~blocked_by:false ~muting:false
          ~muting_notifications:false ~requested:false ~domain_blocking:false
          ~endorsed:false
        |> post_api_v1_accounts_follow_res_to_yojson |> Yojson.Safe.to_string
        |> Result.ok |> Lwt.return
    | _ -> Lwt.return (Error `Internal_server_error)
end
