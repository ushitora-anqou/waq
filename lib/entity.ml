open Util
open Lwt.Infix

(* Entity account *)
type emoji = {
  shortcode : string;
  url : string;
  static_url : string;
  visible_in_picker : bool;
}
[@@deriving make, yojson]

type field = { name : string; value : string; verified_at : string option }
[@@deriving make, yojson]

type credential_account_source = {
  note : string;
  fields : field list;
  privacy : string;
  sensitive : bool;
  language : string;
  follow_requests_count : int;
}
[@@deriving make, yojson]

type account = {
  id : string;
  username : string;
  acct : string;
  url : string;
  display_name : string;
  note : string;
  avatar : string;
  avatar_static : string;
  header : string;
  header_static : string;
  locked : bool;
  fields : field list;
  emojis : emoji list;
  bot : bool;
  group : bool;
  discoverable : bool option;
  created_at : string;
  last_status_at : string option;
  statuses_count : int;
  followers_count : int;
  following_count : int;
  source : credential_account_source option; [@yojson.optional]
}
[@@deriving make, yojson]

let make_account_from_model (a : Db.Account.t) : account Lwt.t =
  let avatar = Config.avatar_url () in
  let header = Config.header_url () in
  let%lwt last_status_at =
    Db.get_last_status_at ~account_id:a.id >|= Option.map Ptime.to_rfc3339
  in
  let%lwt statuses_count = Db.count_statuses ~account_id:a.id in
  let%lwt followers_count = Db.count_followers ~account_id:a.id in
  let%lwt following_count = Db.count_following ~account_id:a.id in
  make_account ~id:(string_of_int a.id) ~username:a.username
    ~acct:(acct a.username a.domain) ~url:a.uri ~display_name:a.display_name
    ~note:"" ~avatar ~avatar_static:avatar ~header ~header_static:header
    ~locked:false ~fields:[] ~emojis:[] ~bot:false ~group:false
    ~discoverable:true
    ~created_at:(Ptime.to_rfc3339 a.created_at)
    ?last_status_at ~statuses_count ~followers_count ~following_count ()
  |> Lwt.return

(* Entity CredentialAccount *)
let make_credential_account_from_model (a : Db.Account.t) : account Lwt.t =
  let source =
    make_credential_account_source ~privacy:"public" ~sensitive:false ~note:""
      ~fields:[] ~language:"ja" ~follow_requests_count:0 ()
  in
  make_account_from_model a >|= fun account ->
  { account with source = Some source }

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
  favourites_count : int;
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
        | exception Sql.NoRowFound -> Lwt.return_none
        | s -> s.account_id |> string_of_int |> Lwt.return_some)
  in
  let%lwt reblog =
    match s.reblog_of_id with
    | None -> Lwt.return_none
    | Some id -> (
        match%lwt Db.Status.get_one ~id () with
        | exception Sql.NoRowFound -> Lwt.return_none
        | s -> make_status_from_model s >|= Option.some)
  in
  let%lwt reblogs_count =
    s.reblog_of_id |> Option.value ~default:s.id |> Db.Status.get_reblogs_count
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
  let%lwt favourites_count =
    Db.Favourite.get_favourites_count ~status_id:s.id
  in
  let%lwt replies_count = Db.Status.get_replies_count s.id in
  let%lwt a = Db.Account.get_one ~id:s.account_id () in
  let%lwt account = make_account_from_model a in
  make_status ~id:(string_of_int s.id)
    ~created_at:(Ptime.to_rfc3339 s.created_at)
    ~visibility ~uri:s.uri ~content:s.text ~account ~replies_count
    ?in_reply_to_id:(s.in_reply_to_id |> Option.map string_of_int)
    ?in_reply_to_account_id ?reblog ~reblogs_count ~reblogged ~favourited
    ~favourites_count ()
  |> Lwt.return

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
[@@deriving make, yojson] [@@yojson.allow_extra_fields]

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

(* Entity notification *)
type notification = {
  id : string;
  typ : string;
  created_at : string;
  account : account;
  status : status option;
}
[@@deriving make]

let yojson_of_notification (r : notification) : Yojson.Safe.t =
  let l =
    [
      ("id", `String r.id);
      ("type", `String r.typ);
      ("created_at", `String r.created_at);
      ("account", yojson_of_account r.account);
    ]
  in
  let l =
    r.status
    |> Option.fold ~none:l ~some:(fun s -> ("status", yojson_of_status s) :: l)
  in
  `Assoc l

let make_notification_from_model ?self_id (m : Db.Notification.t) :
    notification Lwt.t =
  let%lwt account =
    Db.Account.get_one ~id:m.from_account_id () >>= make_account_from_model
  in
  let%lwt status =
    (match (m.activity_type, m.typ) with
    | `Status, Some `reblog ->
        Db.Status.get_one ~id:m.activity_id () >|= Option.some
    | `Favourite, Some `favourite ->
        let%lwt f = Db.Favourite.get_one ~id:m.activity_id () in
        Db.Status.get_one ~id:f.status_id () >|= Option.some
    | `Follow, Some `follow -> Lwt.return_none
    | _ -> assert false)
    >>= Lwt_option.map (make_status_from_model ?self_id)
  in
  make_notification ~id:(string_of_int m.id)
    ~typ:(Option.get m.typ |> Db.Notification.string_of_typ_t)
    ~created_at:(Ptime.to_rfc3339 m.created_at)
    ~account ?status ()
  |> Lwt.return
