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

let account_list_to_hash accounts =
  accounts
  |> List.map (fun (a : account) -> (a.id, a))
  |> List.to_seq |> Hashtbl.of_seq

let in_ints name vals =
  match vals with
  | [] -> None
  | [ x ] -> Some (name ^ " = " ^ string_of_int x)
  | _ ->
      Some
        (name ^ " IN ("
        ^ (vals |> List.map string_of_int |> String.concat ", ")
        ^ ")")

let serialize_account ?(credential = false) (a : Model.Account.t) : account =
  let source =
    (* FIXME *)
    if not credential then None
    else
      make_credential_account_source ~privacy:"public" ~sensitive:false ~note:""
        ~fields:[] ~language:"ja" ~follow_requests_count:0 ()
      |> Option.some
  in
  (* FIXME *)
  let avatar = Config.avatar_url () in
  let header = Config.header_url () in
  make_account
    ~id:(a#id |> Model.Account.ID.to_int |> string_of_int)
    ~username:a#username ~acct:(acct a#username a#domain) ~url:a#uri
    ~display_name:a#display_name ~note:"" ~avatar ~avatar_static:avatar ~header
    ~header_static:header ~locked:false ~fields:[] ~emojis:[] ~bot:false
    ~group:false ~discoverable:true
    ~created_at:(Ptime.to_rfc3339 a#created_at)
    ?last_status_at:(a#stat#last_status_at |> Option.map Ptime.to_rfc3339)
    ~statuses_count:a#stat#statuses_count
    ~followers_count:a#stat#followers_count
    ~following_count:a#stat#following_count ?source ()

let load_accounts_from_db ?(credential = false)
    (account_ids : Model.Account.ID.t list) : account list Lwt.t =
  let%lwt accts = Db.(e Account.(select ~id:(`In account_ids))) in
  Db.(e @@ Account.load_stat accts) >|= fun () ->
  let accts = accts |> index_by (fun x -> x#id) in
  account_ids
  |> List.map (fun id -> Hashtbl.find accts id |> serialize_account ~credential)

let make_account_from_model (a : Db.Account.t) : account Lwt.t =
  load_accounts_from_db [ a#id ] >|= List.hd

let make_credential_account_from_model (a : Db.Account.t) : account Lwt.t =
  load_accounts_from_db ~credential:true [ a#id ] >|= List.hd

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

let status_list_to_hash (statuses : status list) =
  statuses |> List.map (fun s -> (s.id, s)) |> List.to_seq |> Hashtbl.of_seq

let rec serialize_status ?(visibility = "public") (s : Model.Status.t) : status
    =
  let id_to_string x = x |> Model.Status.ID.to_int |> string_of_int in
  make_status ~id:(s#id |> id_to_string)
    ~created_at:(Ptime.to_rfc3339 s#created_at)
    ~visibility ~uri:s#uri ~content:s#text
    ~account:(serialize_account s#account)
    ~replies_count:s#stat#replies_count
    ?in_reply_to_id:(s#in_reply_to_id |> Option.map id_to_string)
    ?in_reply_to_account_id:
      (s#in_reply_to
      |> Option.map @@ fun x ->
         x#account_id |> Model.Account.ID.to_int |> string_of_int)
    ?reblog:(s#reblog_of |> Option.map serialize_status)
    ~reblogs_count:s#stat#reblogs_count
    ~favourites_count:s#stat#favourites_count ~reblogged:s#reblogged
    ~favourited:s#favourited ()

let load_statuses_from_db ?visibility ?self_id
    (status_ids : Model.Status.ID.t list) : status list Lwt.t =
  let%lwt statuses = Db.(e Status.(select ~id:(`In status_ids))) in
  let statuses_plus_reblogs =
    statuses @ List.filter_map (fun s -> s#reblog_of) statuses
  in
  Db.(e @@ Status.load_stat statuses_plus_reblogs);%lwt
  Db.(
    e
    @@ Account.load_stat (statuses_plus_reblogs |> List.map (fun x -> x#account)));%lwt
  Db.(e @@ load_reblogged ?self_id statuses_plus_reblogs);%lwt
  Db.(e @@ load_favourited ?self_id statuses_plus_reblogs);%lwt
  let statuses = statuses |> index_by (fun x -> x#id) in
  status_ids
  |> List.map (fun id ->
         Hashtbl.find statuses id |> serialize_status ?visibility)
  |> Lwt.return

let make_status_from_model ?(visibility = "public") ?self_id (s : Db.Status.t) :
    status Lwt.t =
  load_statuses_from_db ~visibility ?self_id [ s#id ] >|= List.hd

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
  let id = acct#id |> Model.Account.ID.to_int |> string_of_int in
  let%lwt following =
    Db.(e Follow.(does_follow ~account_id:self#id ~target_account_id:acct#id))
  in
  let%lwt followed_by =
    Db.(e Follow.(does_follow ~target_account_id:self#id ~account_id:acct#id))
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

let notification_model_list_to_hash (notis : Db.Notification.t list) =
  notis
  |> List.map (fun (n : Db.Notification.t) -> (n#id, n))
  |> List.to_seq |> Hashtbl.of_seq

let serialize_notifications ?self_id (noti_ids : Model.Notification.ID.t list) :
    notification list Lwt.t =
  let%lwt notis = Db.(e Notification.(select ~id:(`In noti_ids))) in
  let notis_h = notification_model_list_to_hash notis in
  let%lwt accounts_h =
    notis
    |> List.map (fun n -> n#from_account_id)
    |> load_accounts_from_db >|= account_list_to_hash
  in
  let%lwt statuses_fav_h =
    let%lwt favs =
      notis
      |> List.filter_map (fun n ->
             match (n#activity_type, n#typ) with
             | `Favourite, Some `favourite ->
                 Some (n#activity_id |> Model.Favourite.ID.of_int)
             | _ -> None)
      |> fun ids ->
      Db.(e Favourite.(select ~id:(`In ids)))
      >|= List.map (fun f -> (f#id, f#status_id))
    in
    let%lwt statuses = favs |> List.map snd |> load_statuses_from_db ?self_id in
    List.combine (List.map fst favs) statuses
    |> List.to_seq |> Hashtbl.of_seq |> Lwt.return
  in
  let%lwt statuses_reblog_h =
    notis
    |> List.filter_map (fun n ->
           match (n#activity_type, n#typ) with
           | `Status, Some `reblog ->
               Some (n#activity_id |> Model.Status.ID.of_int)
           | _ -> None)
    |> load_statuses_from_db ?self_id
    >|= status_list_to_hash
  in

  noti_ids
  |> List.map (fun noti_id ->
         let m = Hashtbl.find notis_h noti_id in
         let account =
           Hashtbl.find accounts_h
             (m#from_account_id |> Model.Account.ID.to_int |> string_of_int)
         in
         let status =
           match (m#activity_type, m#typ) with
           | `Status, Some `reblog ->
               Some
                 (Hashtbl.find statuses_reblog_h (string_of_int m#activity_id))
           | `Favourite, Some `favourite ->
               Some
                 (Hashtbl.find statuses_fav_h
                    (m#activity_id |> Model.Favourite.ID.of_int))
           | `Follow, Some `follow -> None
           | _ -> assert false
         in
         make_notification
           ~id:(m#id |> Model.Notification.ID.to_int |> string_of_int)
           ~typ:(Option.get m#typ |> Model.Notification.typ_t_to_string)
           ~created_at:(Ptime.to_rfc3339 m#created_at)
           ~account ?status ())
  |> Lwt.return

let make_notification_from_model ?self_id (m : Db.Notification.t) :
    notification Lwt.t =
  serialize_notifications ?self_id [ m#id ] >|= List.hd
