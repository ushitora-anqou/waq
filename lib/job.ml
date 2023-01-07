open Util
module C = Config

exception Bad_request

let context = `String "https://www.w3.org/ns/activitystreams"
let ( ^/ ) s1 s2 = s1 ^ "/" ^ s2

let url (l : string list) =
  "https:/" ^ (C.server_name () :: l |> List.fold_left ( ^/ ) "")

let is_my_domain (u : string) =
  u |> Uri.of_string |> Uri.host
  |> Option.fold ~none:false ~some:(fun h -> C.is_my_domain h)

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

(* Note *)
type ap_note = {
  id : string;
  typ : string; [@key "type"]
  published : string;
  attributedTo : string;
  to_ : string list; [@key "to"]
  cc : string list;
  content : string;
}
[@@deriving make, yojson { strict = false }]

(* Create *)
type ap_create = {
  context : Yojson.Safe.t; [@key "@context"]
  id : string;
  typ : string; [@key "type"]
  actor : Yojson.Safe.t;
  published : string;
  to_ : string list; [@key "to"]
  cc : string list;
  obj : ap_note; [@key "object"]
}
[@@deriving make, yojson { strict = false }]

module ToServer = struct
  (* Send GET /users/:name *)
  let get_uri href =
    let%lwt body =
      Http.fetch_exn ~headers:[ ("Accept", "application/activity+json") ] href
    in
    body |> Yojson.Safe.from_string |> ap_user_of_yojson |> Result.get_ok
    |> Lwt.return

  (* Send GET /.well-known/webfinger *)
  let get_webfinger ~scheme ~domain ~username =
    (* FIXME: Check /.well-known/host-meta if necessary *)
    let%lwt body =
      Http.fetch_exn @@ scheme ^ ":/" ^/ domain
      ^/ ".well-known/webfinger?resource=acct:" ^ username ^ "@" ^ domain
    in
    body |> Yojson.Safe.from_string |> webfinger_of_yojson |> Result.get_ok
    |> Lwt.return

  (* Utility function to get an account. Send GET requests if necessary *)
  let fetch_account ?(scheme = "https") by =
    let make_new_account (uri : string) =
      let%lwt r = get_uri uri in
      let domain = Uri.of_string uri |> Uri.host |> Option.get in
      let now = Unix.gettimeofday () |> Ptime.of_float_s |> Option.get in
      Db.make_account ~username:r.preferredUsername ~domain
        ~public_key:r.publicKey.publicKeyPem ~display_name:r.name ~uri:r.id
        ~url:r.url ~inbox_url:r.inbox ~followers_url:r.followers ~created_at:now
        ~updated_at:now ()
      |> Db.upsert_account
    in
    match by with
    | `Webfinger (domain, username) -> (
        match%lwt Db.get_account_by_username domain username with
        | Some acc -> Lwt.return acc
        | None when domain = "" (* Local *) -> raise Not_found
        | None ->
            let%lwt webfinger = get_webfinger ~scheme ~domain ~username in
            let href =
              webfinger.links
              |> List.find_map (fun l ->
                     match webfinger_link_of_yojson l with
                     | Ok l when l.rel = "self" -> Some l.href
                     | _ -> None)
              |> Option.get
            in
            make_new_account href)
    | `Uri uri -> (
        match%lwt Db.get_account_by_uri uri with
        | Some acc -> Lwt.return acc
        | None -> make_new_account uri)

  (* Send activity+json to POST inbox *)
  let post_activity_to_inbox ~(body : Yojson.Safe.t) ~(src : Db.account)
      ~(dst : Db.account) =
    let body = Yojson.Safe.to_string body in
    let sign =
      let priv_key =
        src.private_key |> Option.get |> Http.Signature.decode_private_key
      in
      let key_id = src.uri ^ "#main-key" in
      let signed_headers =
        [ "(request-target)"; "host"; "date"; "digest"; "content-type" ]
      in
      Some (priv_key, key_id, signed_headers)
    in
    let meth = `POST in
    let headers = [ ("Content-Type", "application/activity+json") ] in
    let%lwt res = Http.fetch ~meth ~headers ~body ~sign dst.inbox_url in
    Lwt.return
    @@
    match res with
    | Ok (status, _body) when Httpaf.Status.is_successful status -> Ok res
    | _ -> Error res

  (* Send Follow to POST inbox *)
  let post_follow_to_inbox self_id id =
    let%lwt self = Db.get_account ~id:self_id in
    let%lwt acc = Db.get_account ~id in
    let body =
      make_ap_inbox ~context ~id:(self.uri ^ "#follow/1") ~typ:"Follow"
        ~actor:(`String self.uri) ~obj:(`String acc.uri)
      |> ap_inbox_to_yojson
    in
    post_activity_to_inbox ~body ~src:self ~dst:acc

  (* Send Create/Note to POST /users/:name/inbox *)
  let post_users_inbox_create_note id (s : Db.status) =
    let%lwt self = Db.get_account ~id:s.account_id in
    let body =
      let published = s.created_at |> Ptime.to_rfc3339 in
      let to_ = [ "https://www.w3.org/ns/activitystreams#Public" ] in
      let cc = [ self.followers_url ] in
      let note =
        make_ap_note ~id:s.uri ~typ:"Note" ~published ~to_ ~cc
          ~attributedTo:self.uri ~content:s.text ()
      in
      make_ap_create ~context ~id:(s.uri ^/ "activity") ~typ:"Create"
        ~actor:(`String self.uri) ~published ~to_ ~cc ~obj:note ()
      |> ap_create_to_yojson
    in
    let%lwt dst = Db.get_account ~id in
    post_activity_to_inbox ~body ~src:self ~dst

  (* Send Accept to POST inbox *)
  let post_accept_to_inbox ~(follow_req : ap_inbox) ~(followee : Db.account)
      ~(follower : Db.account) =
    let id = followee.uri ^ "#accepts/follows/1" in
    let body =
      make_ap_inbox ~context ~id ~typ:"Accept" ~actor:(`String followee.uri)
        ~obj:(follow_req |> ap_inbox_to_yojson)
      |> ap_inbox_to_yojson
    in
    post_activity_to_inbox ~body ~src:followee ~dst:follower
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
      let%lwt a = Db.get_account_by_username "" name in
      let a = Option.get a in
      make_webfinger
        ~subject:("acct:" ^ name ^ "@" ^ dom)
        ~aliases:[ a.uri ]
        ~links:
          [
            make_webfinger_link ~rel:"self" ~typ:"application/activity+json"
              ~href:a.uri
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
      let publicKey =
        make_ap_user_public_key ~id:(a.uri ^ "#main-key") ~owner:a.uri
          ~publicKeyPem:a.public_key
      in
      make_ap_user ~context ~id:a.uri ~typ:"Person"
        ~following:(a.uri ^/ "following") ~followers:a.followers_url
        ~inbox:a.inbox_url ~outbox:(a.uri ^/ "outbox")
        ~preferredUsername:username ~name:a.display_name
        ~summary:"Summary is here" ~url:a.uri ~tag:[] ~publicKey ()
      |> ap_user_to_yojson |> Yojson.Safe.to_string |> Result.ok |> Lwt.return
    with e ->
      Log.debug (fun m ->
          m "[get_users] Can't find user: %s: %s\n%s" username
            (Printexc.to_string e)
            (Printexc.get_backtrace ()));
      Lwt.return (Error `Not_found)

  (* Recv Follow in inbox *)
  let inbox_follow (req : ap_inbox) =
    assert (req.typ = "Follow");
    let src, dst =
      match (req.actor, req.obj) with
      | `String s, `String d when is_my_domain d -> (s, d)
      | _ -> raise Bad_request
    in
    let%lwt src = ToServer.fetch_account (`Uri src) in
    match%lwt Db.get_account_by_uri dst with
    | None -> raise Not_found
    | Some dst ->
        let now = Db.now () in
        (* Insert to table follows *)
        Db.make_follow ~id:0 ~created_at:now ~updated_at:now ~account_id:src.id
          ~target_account_id:dst.id ~uri:req.id
        |> Db.insert_follow |> ignore_lwt;%lwt
        (* Send 'Accept' *)
        ToServer.post_accept_to_inbox ~follow_req:req ~followee:dst
          ~follower:src
        |> ignore_lwt

  (* Recv POST /users/:name/inbox *)
  let post_users_inbox body () =
    match Yojson.Safe.from_string body |> ap_inbox_of_yojson with
    | Error _ -> Lwt.return_unit
    | Ok ({ typ = "Follow"; _ } as r) -> inbox_follow r
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
    let%lwt res = ToServer.post_follow_to_inbox self_id id in
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

  (* Recv GET /api/v1/accounts/search *)
  type get_api_v1_accounts_search_res = {
    id : string;
    username : string;
    acct : string;
    display_name : string;
  }
  [@@deriving make, yojson { strict = false }]

  let get_api_v1_accounts_search _resolve ~username ~domain =
    try%lwt
      let%lwt acc = ToServer.fetch_account (`Webfinger (domain, username)) in
      let acct =
        match acc.domain with
        | None -> username
        | Some _ -> username ^ "@" ^ domain
      in
      make_get_api_v1_accounts_search_res ~id:(string_of_int acc.id) ~username
        ~acct ~display_name:acc.display_name
      |> get_api_v1_accounts_search_res_to_yojson |> Yojson.Safe.to_string
      |> Result.ok |> Lwt.return
    with _ -> Lwt.return (Error `Not_found)

  (* Recv POST /api/v1/statuses *)
  type post_api_v1_statuses_res = {
    id : string;
    created_at : string;
    content : string;
  }
  [@@deriving make, yojson { strict = false }]

  let post_api_v1_statuses self_id status =
    let now = Db.now () in
    let%lwt self = Db.get_account ~id:self_id in
    (* Insert status *)
    let%lwt s =
      Db.make_status ~id:0 ~text:status ~uri:"" ~created_at:now ~updated_at:now
        ~account_id:self_id
      |> Db.insert_status
    in
    (* Update status URI using its ID *)
    let%lwt s =
      { s with uri = self.uri ^/ "statuses" ^/ string_of_int s.id }
      |> Db.update_status_uri
    in
    (* Send followers the status *)
    let%lwt followers = Db.get_follows_by_target_account_id self_id in
    followers
    |> List.iter (fun (f : Db.follow) ->
           Lwt.async @@ fun () ->
           ToServer.post_users_inbox_create_note f.account_id s |> ignore_lwt);
    (* Return the result to the client *)
    make_post_api_v1_statuses_res ~id:(string_of_int s.id)
      ~created_at:(Ptime.to_rfc3339 now) ~content:s.text
    |> post_api_v1_statuses_res_to_yojson |> Yojson.Safe.to_string |> Result.ok
    |> Lwt.return
end
