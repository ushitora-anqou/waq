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

let in_ints name vals =
  match vals with
  | [] -> None
  | [ x ] -> Some (name ^ " = " ^ string_of_int x)
  | _ ->
      Some
        (name ^ " IN ("
        ^ (vals |> List.map string_of_int |> String.concat ", ")
        ^ ")")

let serialize_accounts ?(credential = false) (account_ids : int list) :
    account list Lwt.t =
  let%lwt accts =
    match in_ints "id" account_ids with
    | None -> Lwt.return []
    | Some where -> Db.Account.get_many ~where ()
  in
  let%lwt stats =
    match in_ints "account_id" account_ids with
    | None -> Lwt.return []
    | Some where -> Db.AccountStat.get_many ~where ()
  in
  let avatar = Config.avatar_url () in
  let header = Config.header_url () in
  accts
  |> List.map (fun (a : Db.Account.t) ->
         let open Db.AccountStat in
         let statuses_count, following_count, followers_count, last_status_at =
           stats
           |> List.find_map (fun stat ->
                  if stat.account_id = a.id then
                    Some
                      ( stat.statuses_count,
                        stat.following_count,
                        stat.followers_count,
                        stat.last_status_at )
                  else None)
           |> Option.value ~default:(0, 0, 0, None)
         in
         let last_status_at = last_status_at |> Option.map Ptime.to_rfc3339 in
         let source =
           (* FIXME *)
           if not credential then None
           else
             make_credential_account_source ~privacy:"public" ~sensitive:false
               ~note:"" ~fields:[] ~language:"ja" ~follow_requests_count:0 ()
             |> Option.some
         in
         make_account ~id:(string_of_int a.id) ~username:a.username
           ~acct:(acct a.username a.domain) ~url:a.uri
           ~display_name:a.display_name ~note:"" ~avatar ~avatar_static:avatar
           ~header ~header_static:header ~locked:false ~fields:[] ~emojis:[]
           ~bot:false ~group:false ~discoverable:true
           ~created_at:(Ptime.to_rfc3339 a.created_at)
           ?last_status_at ~statuses_count ~followers_count ~following_count
           ?source ())
  |> Lwt.return

let make_account_from_model (a : Db.Account.t) : account Lwt.t =
  serialize_accounts [ a.id ] >|= List.hd

let make_credential_account_from_model (a : Db.Account.t) : account Lwt.t =
  serialize_accounts ~credential:true [ a.id ] >|= List.hd

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

let serialize_statuses ?(visibility = "public") ?self_id (status_ids : int list)
    : status list Lwt.t =
  let reblog_of_id (s : Db.Status.t) = s.reblog_of_id in
  let id (s : Db.Status.t) = s.id in
  let account_id (s : Db.Status.t) = s.account_id in
  let in_reply_to_id (s : Db.Status.t) = s.in_reply_to_id in
  let status_model_list_to_hash statuses =
    statuses
    |> List.map (fun (s : Db.Status.t) -> (s.id, s))
    |> List.to_seq |> Hashtbl.of_seq
  in
  let stat_model_list_to_hash stats =
    stats
    |> List.map (fun (s : Db.StatusStat.t) -> (s.status_id, s))
    |> List.to_seq |> Hashtbl.of_seq
  in
  let account_list_to_hash accounts =
    accounts
    |> List.map (fun (a : account) -> (a.id, a))
    |> List.to_seq |> Hashtbl.of_seq
  in
  let favourited_list_to_hash favouriteds =
    favouriteds
    |> List.map (fun (f : Db.Favourite.t) -> (f.status_id, 1))
    |> List.to_seq |> Hashtbl.of_seq
  in
  let reblogged_list_to_hash rebloggeds =
    rebloggeds
    |> List.map (fun (s : Db.Status.t) -> (Option.get s.reblog_of_id, 1))
    |> List.sort_uniq compare |> List.to_seq |> Hashtbl.of_seq
  in

  let%lwt statuses = Db.Status.get_many ?where:(in_ints "id" status_ids) () in
  let%lwt reblogs =
    match in_ints "id" (List.filter_map reblog_of_id statuses) with
    | None -> Lwt.return []
    | Some where -> Db.Status.get_many ~where ()
  in
  let all_status_ids =
    reblogs @ statuses |> List.map id |> List.sort_uniq compare
  in
  let%lwt stats =
    match in_ints "status_id" all_status_ids with
    | None -> Lwt.return []
    | Some where -> Db.StatusStat.get_many ~where ()
  in
  let%lwt accounts =
    reblogs @ statuses |> List.map account_id |> List.sort_uniq compare
    |> serialize_accounts
  in
  let%lwt favouriteds, rebloggeds =
    match self_id with
    | None -> Lwt.return ([], [])
    | Some account_id ->
        let where = in_ints "status_id" all_status_ids in
        let%lwt favouriteds = Db.Favourite.get_many ~account_id ?where () in
        let where = in_ints "reblog_of_id" all_status_ids in
        let%lwt rebloggeds = Db.Status.get_many ~account_id ?where () in
        Lwt.return (favouriteds, rebloggeds)
  in
  let%lwt in_reply_to_account_ids_h =
    let in_reply_to_ids =
      reblogs @ statuses |> List.filter_map in_reply_to_id
    in
    match in_ints "id" in_reply_to_ids with
    | None -> Lwt.return (Hashtbl.create 0)
    | Some where ->
        Db.Status.get_many ~where ()
        >|= List.map (fun (s : Db.Status.t) ->
                (s.id, string_of_int s.account_id))
        >|= List.to_seq >|= Hashtbl.of_seq
  in

  let statuses_h = status_model_list_to_hash (reblogs @ statuses) in
  let stats_h = stat_model_list_to_hash stats in
  let accounts_h = account_list_to_hash accounts in
  let favouriteds_h = favourited_list_to_hash favouriteds in
  let rebloggeds_h = reblogged_list_to_hash rebloggeds in

  let rec aux status_id =
    let s = Hashtbl.find statuses_h status_id in
    let account = Hashtbl.find accounts_h (string_of_int s.account_id) in
    let replies_count, reblogs_count, favourites_count =
      let status_id = s.reblog_of_id |> Option.value ~default:status_id in
      match Hashtbl.find_opt stats_h status_id with
      | None -> (0, 0, 0)
      | Some { Db.StatusStat.replies_count; reblogs_count; favourites_count; _ }
        ->
          (replies_count, reblogs_count, favourites_count)
    in
    let in_reply_to_id = s.in_reply_to_id |> Option.map string_of_int in
    let in_reply_to_account_id =
      s.in_reply_to_id |> Option.map (Hashtbl.find in_reply_to_account_ids_h)
    in
    let reblog = s.reblog_of_id |> Option.map aux in
    let reblogged =
      Hashtbl.mem rebloggeds_h status_id
      ||
      match (reblog, self_id) with
      | Some _, Some self_id -> account.id = string_of_int self_id
      | _ -> false
    in
    let favourited = Hashtbl.mem favouriteds_h status_id in
    make_status ~id:(string_of_int s.id)
      ~created_at:(Ptime.to_rfc3339 s.created_at)
      ~visibility ~uri:s.uri ~content:s.text ~account ~replies_count
      ?in_reply_to_id ?in_reply_to_account_id ?reblog ~reblogs_count ~reblogged
      ~favourited ~favourites_count ()
  in

  status_ids |> List.map aux |> Lwt.return

let make_status_from_model ?(visibility = "public") ?self_id (s : Db.Status.t) :
    status Lwt.t =
  serialize_statuses ~visibility ?self_id [ s.id ] >|= List.hd

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
  let%lwt account = serialize_accounts [ m.from_account_id ] >|= List.hd in
  let%lwt status =
    match (m.activity_type, m.typ) with
    | `Status, Some `reblog ->
        serialize_statuses ?self_id [ m.activity_id ]
        >|= List.hd >|= Option.some
    | `Favourite, Some `favourite ->
        let%lwt f = Db.Favourite.get_one ~id:m.activity_id () in
        serialize_statuses ?self_id [ f.status_id ] >|= List.hd >|= Option.some
    | `Follow, Some `follow -> Lwt.return_none
    | _ -> assert false
  in
  make_notification ~id:(string_of_int m.id)
    ~typ:(Option.get m.typ |> Db.Notification.string_of_typ_t)
    ~created_at:(Ptime.to_rfc3339 m.created_at)
    ~account ?status ()
  |> Lwt.return
