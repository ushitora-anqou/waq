open Lwt.Infix
open Util
module Uri = Httpq.Uri

let context = `String "https://www.w3.org/ns/activitystreams"

let url (l : string list) =
  "https:/" ^ (Config.server_name () :: l |> List.fold_left ( ^/ ) "")

let is_my_domain (u : string) =
  u |> Uri.of_string |> Uri.domain |> Config.is_my_domain

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
  inReplyTo : Yojson.Safe.t;
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

(* Get activity+json from the Internet *)
let fetch_activity ~uri =
  Httpq.Client.fetch_exn ~headers:[ (`Accept, "application/activity+json") ] uri
  >|= Yojson.Safe.from_string

(* Send GET /.well-known/webfinger *)
let get_webfinger ~scheme ~domain ~username =
  (* FIXME: Check /.well-known/host-meta if necessary *)
  let%lwt body =
    Httpq.Client.fetch_exn @@ scheme ^ ":/" ^/ domain
    ^/ ".well-known/webfinger?resource=acct:" ^ username ^ "@" ^ domain
  in
  body |> Yojson.Safe.from_string |> webfinger_of_yojson |> Result.get_ok
  |> Lwt.return

(* Utility function to get an account. Send GET requests if necessary *)
let fetch_account ?(scheme = "https") by =
  let make_new_account (uri : string) =
    let%lwt r = fetch_activity ~uri >|= ap_user_of_yojson >|= Result.get_ok in
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
      match%lwt Db.Account.get_one ~domain ~username () with
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
      src.private_key |> Option.get |> Httpq.Signature.decode_private_key
    in
    let key_id = src.uri ^ "#main-key" in
    let signed_headers =
      [ "(request-target)"; "host"; "date"; "digest"; "content-type" ]
    in
    Some (priv_key, key_id, signed_headers)
  in
  let meth = `POST in
  let headers = [ (`Content_type, "application/activity+json") ] in
  let%lwt res = Httpq.Client.fetch ~meth ~headers ~body ~sign dst.inbox_url in
  Lwt.return
  @@
  match res with
  | Ok (status, _, _body)
    when Cohttp.Code.(status |> code_of_status |> is_success) ->
      ()
  | _ -> failwith "Failed to post activity to inbox"

(* Conversion functions *)
let ap_create_note_from_model (s : Db.Status.t) =
  let%lwt self = Db.Account.get_one ~id:s.account_id () in
  let%lwt in_reply_to_s =
    match s.in_reply_to_id with
    | None -> Lwt.return_none
    | Some id -> Db.Status.get_one ~id () >|= Option.some
  in
  let published = s.created_at |> Ptime.to_rfc3339 in
  let to_ = [ "https://www.w3.org/ns/activitystreams#Public" ] in
  let cc = [ self.followers_url ] in
  let inReplyTo =
    Db.Status.(
      in_reply_to_s |> Option.fold ~none:`Null ~some:(fun s -> `String s.uri))
  in
  let note =
    make_ap_note ~id:s.uri ~typ:"Note" ~published ~to_ ~cc
      ~attributedTo:self.uri ~content:s.text ~inReplyTo ()
  in
  make_ap_create ~context ~id:(s.uri ^/ "activity") ~typ:"Create"
    ~actor:(`String self.uri) ~published ~to_ ~cc ~obj:note ()
  |> Lwt.return

let rec ap_create_note_to_model (req : ap_create) : Db.Status.t Lwt.t =
  let note = req.obj in
  let published, _, _ = Ptime.of_rfc3339 note.published |> Result.get_ok in
  let%lwt in_reply_to_id =
    let uri_to_status_id uri =
      fetch_status ~uri >|= fun (s : Db.Status.t) -> Some s.id
    in
    match note.inReplyTo with
    | `String uri -> uri_to_status_id uri
    | `Assoc l -> (
        match l |> List.assoc_opt "id" with
        | Some (`String uri) -> uri_to_status_id uri
        | _ -> Lwt.return_none)
    | _ -> Lwt.return_none
  in
  let%lwt attributedTo = fetch_account (`Uri note.attributedTo) in
  Db.Status.(
    make ~id:0 ~uri:note.id ~text:note.content ~created_at:published
      ~updated_at:published ~account_id:attributedTo.id ?in_reply_to_id ()
    |> save_one)

and fetch_status ~uri =
  match%lwt Db.Status.get_one ~uri () with
  | s -> Lwt.return s
  | exception Sql.NoRowFound ->
      fetch_activity ~uri >|= ap_create_of_yojson >|= Result.get_ok
      >>= ap_create_note_to_model >>= Db.Status.save_one
