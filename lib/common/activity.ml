open Util

exception Bad_request
exception Internal_server_error

let context = `String "https://www.w3.org/ns/activitystreams"
let ( ^/ ) s1 s2 = s1 ^ "/" ^ s2

let url (l : string list) =
  "https:/" ^ (Config.server_name () :: l |> List.fold_left ( ^/ ) "")

let is_my_domain (u : string) =
  u |> Uri.of_string |> Uri.host
  |> Option.fold ~none:false ~some:(fun h -> Config.is_my_domain h)

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

type ap_inbox_no_context = {
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
    let now = Ptime.now () in
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
  | Ok (status, _body) when Httpaf.Status.is_successful status -> ()
  | _ -> raise Internal_server_error
