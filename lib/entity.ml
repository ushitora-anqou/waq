open Util

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

(* Entity CredentialAccount *)
type credential_account_source = { privacy : string; sensitive : bool }
[@@deriving make, yojson]

type credential_account = {
  id : string;
  username : string;
  acct : string;
  display_name : string;
  created_at : string;
  source : credential_account_source;
}
[@@deriving make, yojson]

let make_credential_account_from_model (a : Db.Account.t) : credential_account =
  let source =
    make_credential_account_source ~privacy:"public" ~sensitive:false
  in
  make_credential_account ~id:(string_of_int a.id) ~username:a.username
    ~acct:(acct a.username a.domain) ~display_name:a.display_name
    ~created_at:(Ptime.to_rfc3339 a.created_at)
    ~source

(* Entity status *)
type status_mention = {
  id : string;
  username : string;
  url : string;
  acct : string;
}
[@@deriving make, yojson]

type status = {
  id : string;
  created_at : string;
  visibility : string;
  uri : string;
  content : string;
  replies_count : int;
  in_reply_to_id : string option;
  in_reply_to_account_id : string option;
  account : account;
  mentions : status_mention list;
  reblog : status option;
  reblogs_count : int;
  reblogged : bool;
  favourited : bool;
}
[@@deriving make, yojson]

let rec make_status_from_model ?(visibility = "public") ?self_id
    (s : Db.Status.t) : status Lwt.t =
  let open Lwt.Infix in
  let make_status_from_model = make_status_from_model ~visibility ?self_id in
  let%lwt in_reply_to_account_id =
    match s.in_reply_to_id with
    | None -> Lwt.return_none
    | Some id -> (
        match%lwt Db.Status.get_one ~id () with
        | exception Sql.NoRowFound ->
            Httpq.Server.raise_error_response `Bad_request
        | s -> s.account_id |> string_of_int |> Lwt.return_some)
  in
  let%lwt reblog, reblogs_count =
    match s.reblog_of_id with
    | None -> Lwt.return (None, 0)
    | Some id ->
        let%lwt count = Db.Status.get_reblogs_count id in
        let%lwt reblog = Db.Status.get_one ~id () >>= make_status_from_model in
        Lwt.return (Some reblog, count)
  in
  let%lwt reblogged =
    match self_id with
    | None -> Lwt.return_false
    | Some account_id when reblog <> None ->
        Lwt.return (s.account_id = account_id)
    | Some account_id ->
        Db.Status.get_many ~account_id ~reblog_of_id:(Some s.id) ()
        >|= ( <> ) []
  in
  let%lwt favourited =
    match self_id with
    | None -> Lwt.return_false
    | Some account_id ->
        Db.Favourite.get_many ~account_id ~status_id:s.id () >|= ( <> ) []
  in
  let%lwt replies_count = Db.Status.get_replies_count s.id in
  Db.Account.get_one ~id:s.account_id () >|= fun a ->
  let account = make_account_from_model a in
  make_status ~id:(string_of_int s.id)
    ~created_at:(Ptime.to_rfc3339 s.created_at)
    ~visibility ~uri:s.uri ~content:s.text ~account ~replies_count
    ?in_reply_to_id:(s.in_reply_to_id |> Option.map string_of_int)
    ?in_reply_to_account_id ?reblog ~reblogs_count ~reblogged ~favourited ()

(* Entity relationship *)
type relationship = {
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
  note : string;
}
[@@deriving make, yojson { strict = false }]

let make_relationship_from_model (self : Db.Account.t) (acct : Db.Account.t) =
  let id = acct.id |> string_of_int in
  let%lwt following =
    Db.Follow.does_follow ~account_id:self.id ~target_account_id:acct.id
  in
  let%lwt followed_by =
    Db.Follow.does_follow ~target_account_id:self.id ~account_id:acct.id
  in
  make_relationship ~id ~following ~followed_by ~showing_reblogs:true
    ~notifying:false ~blocking:false ~blocked_by:false ~muting:false
    ~muting_notifications:false ~requested:false ~domain_blocking:false
    ~endorsed:false ~note:""
  |> Lwt.return
