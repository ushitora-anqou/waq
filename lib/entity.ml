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
  let avatar =
    a#avatar_remote_url |> Option.value ~default:(Config.notfound_avatar_url ())
  in
  let header =
    if a#header_remote_url = "" then Config.notfound_header_url ()
    else a#header_remote_url
  in
  let stat =
    let default =
      Model.AccountStat.make ~account_id:a#id ~statuses_count:0
        ~following_count:0 ~followers_count:0 ()
    in
    a#stat |> Option.value ~default
  in
  make_account
    ~id:(a#id |> Model.Account.ID.to_int |> string_of_int)
    ~username:a#username ~acct:(acct a#username a#domain) ~url:a#uri
    ~display_name:a#display_name ~note:"" ~avatar ~avatar_static:avatar ~header
    ~header_static:header ~locked:false ~fields:[] ~emojis:[] ~bot:false
    ~group:false ~discoverable:true
    ~created_at:(Ptime.to_rfc3339 a#created_at)
    ?last_status_at:(stat#last_status_at |> Option.map Ptime.to_rfc3339)
    ~statuses_count:stat#statuses_count ~followers_count:stat#followers_count
    ~following_count:stat#following_count ?source ()

let load_accounts_from_db ?(credential = false)
    (account_ids : Model.Account.ID.t list) : account list Lwt.t =
  Db.(e Account.(select ~id:(`In account_ids) ~preload:[ `stat [] ]))
  >|= index_by (fun x -> x#id)
  >|= fun accts ->
  account_ids
  |> List.map (fun id -> Hashtbl.find accts id |> serialize_account ~credential)

let make_account_from_model (a : Db.Account.t) : account Lwt.t =
  load_accounts_from_db [ a#id ] >|= List.hd

let make_credential_account_from_model (a : Db.Account.t) : account Lwt.t =
  load_accounts_from_db ~credential:true [ a#id ] >|= List.hd

(* Entity MediaAttachment *)
type media_attachment_meta = MAImage
(* of {
     original_width : int;
     original_height : int;
     original_size : string;
     original_aspect : float;
     small_width : int;
     small_height : int;
     small_size : string;
     small_aspect : float;
     focus_x : float;
     focus_y : float;
   }*)

type media_attachment = {
  id : string;
  url : string;
  preview_url : string;
  remote_url : string option;
  meta : media_attachment_meta;
  description : string option;
  blurhash : string;
}
[@@deriving make]

let yojson_of_media_attachment (ma : media_attachment) : Yojson.Safe.t =
  let l =
    [
      ("id", `String ma.id);
      ("url", `String ma.url);
      ("preview_url", `String ma.preview_url);
      ( "remote_url",
        ma.remote_url |> Option.fold ~none:`Null ~some:(fun s -> `String s) );
      ( "description",
        ma.description |> Option.fold ~none:`Null ~some:(fun s -> `String s) );
      ("blurhash", `String ma.blurhash);
    ]
  in
  let l =
    ("type", `String "image") :: ("meta", `Assoc []) :: l
    (*
    match ma.meta with
    | MAImage r ->
        let meta =
          [
            ( "original",
              `Assoc
                [
                  ("width", `Int r.original_width);
                  ("height", `Int r.original_height);
                  ("size", `String r.original_size);
                  ("aspect", `Float r.original_aspect);
                ] );
            ( "small",
              `Assoc
                [
                  ("width", `Int r.small_width);
                  ("height", `Int r.small_height);
                  ("size", `String r.small_size);
                  ("aspect", `Float r.small_aspect);
                ] );
            ( "focus",
              `Assoc [ ("x", `Float r.focus_x); ("y", `Float r.focus_y) ] );
          ]
        in
        ("type", `String "image") :: ("meta", `Assoc meta) :: l
        *)
  in
  `Assoc l

let serialize_media_attachment (ma : Model.MediaAttachment.t) : media_attachment
    =
  match ma#type_ with
  | 0 ->
      let blurhash = "LlMF%n00%#MwS|WCWEM{R*bbWBbH" (* FIXME *) in
      let meta = MAImage in
      make_media_attachment
        ~id:(ma#id |> Model.MediaAttachment.ID.to_int |> string_of_int)
        ~url:ma#remote_url ~preview_url:ma#remote_url ~meta ~blurhash ()
  | _ -> failwith "Invalid type of media attachment"

(* Entity status *)
type status_mention = {
  id : string;
  username : string;
  url : string;
  acct : string;
}
[@@deriving make, yojson]

let serialize_mention (m : Model.Mention.t) : status_mention =
  let a = Option.get m#account in
  make_status_mention
    ~id:(Model.Account.ID.to_int a#id |> string_of_int)
    ~username:a#username ~url:a#uri ~acct:(acct a#username a#domain)

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
  media_attachments : media_attachment list;
  spoiler_text : string;
}
[@@deriving make, yojson_of]

let status_list_to_hash (statuses : status list) =
  statuses |> List.map (fun s -> (s.id, s)) |> List.to_seq |> Hashtbl.of_seq

let rec serialize_status ?(visibility = "public") (s : Model.Status.t) : status
    =
  let id_to_string x = x |> Model.Status.ID.to_int |> string_of_int in
  let stat =
    let default =
      Model.StatusStat.make ~status_id:s#id ~replies_count:0 ~reblogs_count:0
        ~favourites_count:0 ()
    in
    match s#reblog_of_id with
    | None -> s#stat |> Option.value ~default
    | Some _ -> (Option.get s#reblog_of)#stat |> Option.value ~default
  in
  make_status ~id:(s#id |> id_to_string)
    ~created_at:(Ptime.to_rfc3339 s#created_at)
    ~visibility ~uri:s#uri ~content:s#text
    ~account:(serialize_account s#account)
    ~replies_count:stat#replies_count
    ?in_reply_to_id:(s#in_reply_to_id |> Option.map id_to_string)
    ?in_reply_to_account_id:
      (s#in_reply_to
      |> Option.map @@ fun x ->
         x#account_id |> Model.Account.ID.to_int |> string_of_int)
    ?reblog:(s#reblog_of |> Option.map serialize_status)
    ~reblogs_count:stat#reblogs_count ~favourites_count:stat#favourites_count
    ~reblogged:s#reblogged ~favourited:s#favourited
    ~media_attachments:(s#attachments |> List.map serialize_media_attachment)
    ~mentions:(s#mentions |> List.map serialize_mention)
    ~spoiler_text:s#spoiler_text ()

let status_preload_spec self_id : Model.Status.preload_spec =
  [
    `stat [];
    `account [ `stat [] ];
    `in_reply_to [];
    `reblogged self_id;
    `favourited self_id;
    `attachments [];
    `mentions [ `account [] ];
    `reblog_of
      [
        `stat [];
        `account [ `stat [] ];
        `in_reply_to [];
        `reblog_of [];
        `reblogged self_id;
        `favourited self_id;
        `attachments [];
        `mentions [ `account [] ];
      ];
  ]

let load_statuses_from_db ?visibility ?(self_id : Model.Account.ID.t option)
    (status_ids : Model.Status.ID.t list) : status list Lwt.t =
  let%lwt statuses =
    Db.(
      e
        Status.(
          select ~id:(`In status_ids) ~preload:(status_preload_spec self_id)))
  in
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

let serialize_notification (n : Model.Notification.t) : notification =
  make_notification
    ~id:(n#id |> Model.Notification.ID.to_int |> string_of_int)
    ~typ:(Option.get n#typ |> Model.Notification.typ_t_to_string)
    ~created_at:(Ptime.to_rfc3339 n#created_at)
    ~account:(serialize_account n#from_account)
    ?status:(n#target_status |> Option.map serialize_status)
    ()

let load_notifications_from_db ?self_id
    (noti_ids : Model.Notification.ID.t list) : notification list Lwt.t =
  Db.(
    e
      Notification.(
        select ~id:(`In noti_ids)
          ~preload:
            [
              `from_account [ `stat [] ];
              `target_status (status_preload_spec self_id);
            ]))
  >|= index_by (fun x -> x#id)
  >|= fun notis ->
  noti_ids
  |> List.map (fun id -> Hashtbl.find notis id |> serialize_notification)

(* Entity marker *)
type marker = { last_read_id : string; version : int; updated_at : string }
[@@deriving make, yojson]

let serialize_marker (x : Model.Marker.t) : marker =
  make_marker
    ~last_read_id:(string_of_int x#last_read_id)
    ~version:1 (* FIXME: dummy *)
    ~updated_at:(Ptime.to_rfc3339 x#updated_at)
