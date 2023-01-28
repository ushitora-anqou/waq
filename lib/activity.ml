open Util
module Uri = Http.Uri

let context = `String "https://www.w3.org/ns/activitystreams"
let ( ^/ ) s1 s2 = s1 ^ "/" ^ s2

let url (l : string list) =
  "https:/" ^ (Config.server_name () :: l |> List.fold_left ( ^/ ) "")

let is_my_domain (u : string) =
  u |> Uri.of_string |> Uri.domain |> Config.is_my_domain

let acct (username : string) (domain : string option) : string =
  match domain with None -> username | Some domain -> username ^ "@" ^ domain

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
    let domain = Uri.of_string uri |> Uri.domain in
    let now = Ptime.now () in
    Db.Account.make ~username:r.preferredUsername ~domain
      ~public_key:r.publicKey.publicKeyPem ~display_name:r.name ~uri:r.id
      ~url:r.url ~inbox_url:r.inbox ~followers_url:r.followers ~created_at:now
      ~updated_at:now ()
    |> Db.Account.save_one
  in
  match by with
  | `Webfinger (domain, username) -> (
      match%lwt Db.Account.get_one_by_domain_and_username ~domain ~username with
      | acc -> Lwt.return acc
      | exception Sql.NoRowFound when domain = None (* Local *) ->
          failwith "Couldn't fetch the account"
      | exception Sql.NoRowFound ->
          let domain = Option.get domain in
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
      match%lwt Db.Account.get_one ~uri () with
      | acc -> Lwt.return acc
      | exception Sql.NoRowFound -> make_new_account uri)

(* Send activity+json to POST inbox *)
let post_activity_to_inbox ~(body : Yojson.Safe.t) ~(src : Db.Account.t)
    ~(dst : Db.Account.t) =
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
  | Ok (status, _, _body)
    when Cohttp.Code.(status |> code_of_status |> is_success) ->
      ()
  | _ -> failwith "Failed to post activity to inbox"

(* Entity account *)
type account = {
  id : string;
  username : string;
  acct : string;
  display_name : string;
  created_at : string;
}
[@@deriving make, yojson]

let make_account_from_model (a : Db.Account.t) =
  make_account ~id:(string_of_int a.id) ~username:a.username
    ~acct:(acct a.username a.domain) ~display_name:a.display_name
    ~created_at:(Ptime.to_rfc3339 a.created_at)

(* Entity status *)
type status = {
  id : string;
  created_at : string;
  visibility : string;
  uri : string;
  content : string;
  account : account;
}
[@@deriving make, yojson]

let make_status_from_model ?(visibility = "public") (s : Db.Status.t) =
  let open Lwt.Infix in
  Db.Account.get_one ~id:s.account_id () >|= fun a ->
  let account = make_account_from_model a in
  make_status ~id:(string_of_int s.id)
    ~created_at:(Ptime.to_rfc3339 s.created_at)
    ~visibility ~uri:s.uri ~content:s.text ~account
