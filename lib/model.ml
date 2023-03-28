open Util
open Lwt.Infix
open Schema

module AccountStat = struct
  include AccountStat

  let increment ~account_id ?(statuses_count = 0) ?(following_count = 0)
      ?(followers_count = 0) ?last_status_at (c : Sqlx.Connection.t) =
    c#execute
      {|
INSERT INTO account_stats ( account_id, statuses_count, following_count, followers_count, created_at, updated_at, last_status_at )
VALUES ( $1, $2, $3, $4, now(), now(), $5 )
ON CONFLICT (account_id) DO UPDATE
SET statuses_count  = account_stats.statuses_count + $2,
    following_count = account_stats.following_count + $3,
    followers_count = account_stats.followers_count + $4,
    last_status_at = $5,
    updated_at = now()|}
      ~p:
        [
          `Int (Account.ID.to_int account_id);
          `Int statuses_count;
          `Int following_count;
          `Int followers_count;
          `NullTimestamp last_status_at;
        ]

  let decrement ~account_id ?(statuses_count = 0) ?(following_count = 0)
      ?(followers_count = 0) c =
    let statuses_count = -statuses_count in
    let following_count = -following_count in
    let followers_count = -followers_count in
    increment ~account_id ~statuses_count ~following_count ~followers_count c
end

module Account = struct
  include Account

  let is_local (x : t) = Option.is_none x#domain
  let is_remote (x : t) = Option.is_some x#domain

  let preferred_inbox_url (a : t) =
    match (a#inbox_url, a#shared_inbox_url) with s, "" -> s | _, s -> s

  let preferred_inbox_urls (accts : t list) =
    accts |> List.map preferred_inbox_url |> List.sort_uniq compare
end

module StatusStat = struct
  include StatusStat

  let increment ~status_id ?(replies_count = 0) ?(reblogs_count = 0)
      ?(favourites_count = 0) (c : Sqlx.Connection.t) =
    c#execute
      {|
INSERT INTO status_stats ( status_id, replies_count, reblogs_count, favourites_count, created_at, updated_at )
VALUES ($1, $2, $3, $4, now(), now())
ON CONFLICT (status_id) DO UPDATE
SET replies_count = status_stats.replies_count + $2,
    reblogs_count = status_stats.reblogs_count + $3,
    favourites_count = status_stats.favourites_count + $4,
    updated_at = now()|}
      ~p:
        [
          `Int (Status.ID.to_int status_id);
          `Int replies_count;
          `Int reblogs_count;
          `Int favourites_count;
        ]

  let decrement ~status_id ?(replies_count = 0) ?(reblogs_count = 0)
      ?(favourites_count = 0) c =
    let replies_count = -replies_count in
    let reblogs_count = -reblogs_count in
    let favourites_count = -favourites_count in
    increment ~status_id ~replies_count ~reblogs_count ~favourites_count c
end

module Status = struct
  include Status

  let get_one' = get_one
  let get_many' = get_many
  let get_one = get_one ~deleted_at:None
  let get_many = get_many ~deleted_at:None

  let save_one_with_uri s c =
    let%lwt s = save_one s c ~preload:[ `account [] ] in
    let self = s#account in
    let uri = self#uri ^/ "statuses" ^/ string_of_int (ID.to_int s#id) in
    let s = s#with_uri uri in
    update [ s ] c >|= Sqlx.Ppx_runtime.expect_single_row

  let get_reblogs_count id c : int Lwt.t =
    count ~deleted_at:`EqNone ~reblog_of_id:(`Eq id) c

  let get_replies_count id c : int Lwt.t =
    count ~deleted_at:`EqNone ~in_reply_to_id:(`Eq id) c

  let get_ancestors (id : ID.t) (c : Sqlx.Connection.t) : t list Lwt.t =
    c#query
      ~p:[ `Int (ID.to_int id) ]
      {|
WITH RECURSIVE t(id) AS (
  SELECT in_reply_to_id FROM statuses WHERE deleted_at IS NULL AND id = $1
  UNION
  SELECT in_reply_to_id FROM statuses s, t WHERE deleted_at IS NULL AND s.id = t.id
)
SELECT * FROM statuses WHERE id IN (SELECT * FROM t)|}
    >|= List.map pack

  let get_descendants (id : ID.t) (c : Sqlx.Connection.t) : t list Lwt.t =
    c#query
      ~p:[ `Int (ID.to_int id) ]
      {|
WITH RECURSIVE t(id) AS (
  SELECT id FROM statuses WHERE deleted_at IS NULL AND in_reply_to_id = $1
  UNION
  SELECT s.id FROM statuses s, t WHERE deleted_at IS NULL AND s.in_reply_to_id = t.id
)
SELECT * FROM statuses WHERE id IN (SELECT * FROM t)|}
    >|= List.map pack

  let after_discard_with_reblogs_callbacks :
      (t -> Sqlx.Connection.t -> unit Lwt.t) list ref =
    ref []

  let after_discard_with_reblogs f =
    let r = after_discard_with_reblogs_callbacks in
    r := f :: !r

  let discard_with_reblogs (id : ID.t) c : t Lwt.t =
    let now = Ptime.now () in
    let%lwt reblogs =
      get_many ~reblog_of_id:(Some id) c
      >|= List.map (fun s -> s#with_deleted_at (Some now))
      >>= fun xs -> update xs c
    in
    let%lwt status =
      (get_one ~id c >|= fun s -> s#with_deleted_at (Some now)) >>= fun x ->
      update [ x ] c >|= function [ x ] -> x | _ -> assert false
    in
    (status :: reblogs
    |> Lwt_list.iter_s @@ fun s ->
       !after_discard_with_reblogs_callbacks
       |> Lwt_list.iter_s @@ fun f -> f s c);%lwt
    Lwt.return status

  let () =
    (* Set callbacks for State *)
    after_create_commit (fun s c ->
        AccountStat.increment ~account_id:s#account_id ~statuses_count:1
          ~last_status_at:s#created_at c;%lwt
        s#reblog_of_id
        |> Lwt_option.iter (fun status_id ->
               StatusStat.increment ~status_id ~reblogs_count:1 c);%lwt
        s#in_reply_to_id
        |> Lwt_option.iter (fun status_id ->
               StatusStat.increment ~status_id ~replies_count:1 c);%lwt
        Lwt.return_unit);
    after_discard_with_reblogs (fun s c ->
        AccountStat.decrement ~account_id:s#account_id ~statuses_count:1 c;%lwt
        s#reblog_of_id
        |> Lwt_option.iter (fun status_id ->
               StatusStat.decrement ~status_id ~reblogs_count:1 c);%lwt
        s#in_reply_to_id
        |> Lwt_option.iter (fun status_id ->
               StatusStat.decrement ~status_id ~replies_count:1 c);%lwt
        Lwt.return_unit)

  let () =
    loader_reblogged :=
      fun ?preload:(self_id = None) (xs : Status.t list) c ->
        select
          ?account_id:(self_id |> Option.map @@ fun x -> `Eq x)
          ~reblog_of_id:(`In (xs |> List.map (fun x -> x#id)))
          c
        >|= index_by (fun x -> Option.get x#reblog_of_id)
        >|= fun tbl ->
        xs
        |> List.iter @@ fun (x : t) ->
           (Hashtbl.mem tbl x#id
           ||
           match (x#reblog_of_id, self_id) with
           | Some _, Some self_id -> x#account_id = self_id
           | _ -> false)
           |> Option.some |> x#set_reblogged

  let () =
    (loader_favourited :=
       fun ?preload:(self_id = None) (xs : Status.t list) c ->
         Favourite.select
           ?account_id:(self_id |> Option.map @@ fun x -> `Eq x)
           ~status_id:(`In (xs |> List.map (fun x -> x#id)))
           c
         >|= index_by (fun x -> x#status_id)
         >|= fun tbl ->
         xs
         |> List.iter @@ fun (x : t) ->
            Hashtbl.mem tbl x#id |> Option.some |> x#set_favourited);
    ()
end

module User = struct
  include User
end

module Follow = struct
  include Follow

  let does_follow ~account_id ~target_account_id c : bool Lwt.t =
    get_many ~account_id ~target_account_id c >|= ( <> ) []

  let () =
    after_create_commit (fun f c ->
        let open AccountStat in
        increment ~account_id:f#account_id ~following_count:1 c;%lwt
        increment ~account_id:f#target_account_id ~followers_count:1 c);
    after_destroy_commit (fun f c ->
        let open AccountStat in
        decrement ~account_id:f#account_id ~following_count:1 c;%lwt
        decrement ~account_id:f#target_account_id ~followers_count:1 c)
end

module FollowRequest = struct
  include FollowRequest
end

module OAuthApplication = struct
  include OAuthApplication
end

module OAuthAccessGrant = struct
  include OAuthAccessGrant
end

module OAuthAccessToken = struct
  include OAuthAccessToken
end

module Favourite = struct
  include Favourite

  let get_favourites_count ~status_id c = count ~status_id:(`Eq status_id) c

  let () =
    after_create_commit (fun f c ->
        StatusStat.increment ~status_id:f#status_id ~favourites_count:1 c);
    after_destroy_commit (fun f c ->
        StatusStat.decrement ~status_id:f#status_id ~favourites_count:1 c)
end

module MediaAttachment = struct
  include MediaAttachment
end

module Notification = struct
  include Notification

  let () =
    loader_target_status :=
      fun ?preload (ns : t list) c ->
        let map_fst_sort_uniq r = r |> List.map fst |> List.sort_uniq compare in

        (* Load default values i.e., None *)
        ns |> List.iter (fun n -> n#set_target_status (Some None));

        (* Load favourites *)
        (let targets =
           ns
           |> List.filter_map (fun n ->
                  match (n#activity_type, n#typ) with
                  | `Favourite, _ | _, Some `favourite ->
                      Some
                        (Favourite.ID.of_int n#activity_id, n#set_target_status)
                  | _ -> None)
         in
         let%lwt tbl =
           Favourite.select ~id:(`In (map_fst_sort_uniq targets)) c
           >|= index_by (fun x -> x#id)
         in
         let targets =
           targets
           |> List.map (fun (fav_id, f) ->
                  ((Hashtbl.find tbl fav_id)#status_id, f))
         in
         Status.select ?preload ~id:(`In (map_fst_sort_uniq targets)) c
         >|= index_by (fun x -> x#id)
         >|= fun tbl ->
         targets
         |> List.iter @@ fun (status_id, f) ->
            f (Some (Some (Hashtbl.find tbl status_id))));%lwt

        (* Load reblogs *)
        (let targets =
           ns
           |> List.filter_map (fun n ->
                  match (n#activity_type, n#typ) with
                  | `Status, _ | _, Some `reblog ->
                      Some (Status.ID.of_int n#activity_id, n#set_target_status)
                  | _ -> None)
         in
         Status.select ?preload ~id:(`In (map_fst_sort_uniq targets)) c
         >|= index_by (fun x -> x#id)
         >|= fun tbl ->
         targets
         |> List.iter (fun (status_id, f) ->
                f (Some (Some (Hashtbl.find tbl status_id)))));%lwt

        (* Load mentions *)
        (let targets =
           ns
           |> List.filter_map (fun n ->
                  match (n#activity_type, n#typ) with
                  | `Mention, _ | _, Some `mention ->
                      Some (Mention.ID.of_int n#activity_id, n#set_target_status)
                  | _ -> None)
         in
         let%lwt tbl =
           Mention.select ~id:(`In (map_fst_sort_uniq targets)) c
           >|= index_by (fun x -> x#id)
         in
         let targets =
           targets
           |> List.filter_map (fun (men_id, f) ->
                  (Hashtbl.find tbl men_id)#status_id
                  |> Option.map (fun id -> (id, f)))
         in
         Status.select ?preload ~id:(`In (map_fst_sort_uniq targets)) c
         >|= index_by (fun x -> x#id)
         >|= fun tbl ->
         targets
         |> List.iter @@ fun (status_id, f) ->
            f (Some (Some (Hashtbl.find tbl status_id))));%lwt

        Lwt.return_unit
end

module Mention = struct
  include Mention
end

let home_timeline ~id ~limit ~max_id ~since_id (c : Sqlx.Connection.t) :
    Status.t list Lwt.t =
  c#query
    {|
SELECT * FROM statuses s
WHERE
  deleted_at IS NULL AND
  ( account_id = $1 OR
    account_id IN (SELECT target_account_id FROM follows WHERE account_id = $1) OR
    EXISTS (SELECT 1 FROM mentions WHERE status_id = s.id AND account_id = $1) ) AND
  ( $3 = 0 OR id >= $3 ) AND ( $4 = 0 OR id <= $4 )
ORDER BY created_at DESC LIMIT $2|}
    ~p:
      [
        `Int (Account.ID.to_int id);
        `Int limit;
        `Int (Option.fold ~none:0 ~some:Status.ID.to_int since_id);
        `Int (Option.fold ~none:0 ~some:Status.ID.to_int max_id);
      ]
  >|= List.map Status.pack

let account_statuses ~id ~limit ~max_id ~since_id ~exclude_replies
    (c : Sqlx.Connection.t) : Status.t list Lwt.t =
  let where =
    [
      "deleted_at IS NULL";
      "account_id = :id";
      ":since_id = 0 OR id >= :since_id";
      ":max_id = 0 OR id <= :max_id";
    ]
  in
  let where =
    if exclude_replies then "in_reply_to_id IS NULL" :: where else where
  in
  let sql =
    where
    |> List.map (fun x -> "(" ^ x ^ ")")
    |> String.concat " AND "
    |> Printf.sprintf
         {|SELECT * FROM statuses WHERE %s ORDER BY created_at DESC LIMIT :limit|}
  in
  c#named_query sql
    ~p:
      [
        ("id", `Int (Account.ID.to_int id));
        ("limit", `Int limit);
        ("since_id", `Int (Option.fold ~none:0 ~some:Status.ID.to_int since_id));
        ("max_id", `Int (Option.fold ~none:0 ~some:Status.ID.to_int max_id));
      ]
  >|= List.map Status.pack

let get_favourited_by ~status_id c =
  let%lwt xs = Favourite.get_many ~status_id c in
  let ids = xs |> List.map (fun x -> x#account_id) in
  Account.select ~id:(`In ids) c

let count_followers ~account_id c =
  Follow.count ~target_account_id:(`Eq account_id) c

let count_following ~account_id c = Follow.count ~account_id:(`Eq account_id) c

let get_followers ~id ~self_id:_ ~max_id ~since_id ~limit
    (c : Sqlx.Connection.t) : Account.t list Lwt.t =
  c#named_query
    {|
SELECT a.* FROM accounts a
INNER JOIN follows f ON a.id = f.account_id
WHERE f.target_account_id = :id
AND (:since_id = 0 OR f.id >= :since_id)
AND (:max_id = 0 OR f.id <= :max_id)
ORDER BY f.created_at DESC
LIMIT :limit|}
    ~p:
      [
        ("id", `Int (Account.ID.to_int id));
        ("limit", `Int limit);
        ("since_id", `Int (Option.fold ~none:0 ~some:Follow.ID.to_int since_id));
        ("max_id", `Int (Option.fold ~none:0 ~some:Follow.ID.to_int max_id));
      ]
  >|= List.map Account.pack

let get_following ~id ~self_id:_ ~max_id ~since_id ~limit
    (c : Sqlx.Connection.t) : Account.t list Lwt.t =
  c#named_query
    {|
SELECT a.* FROM accounts a
INNER JOIN follows f ON a.id = f.target_account_id
WHERE f.account_id = :id
AND (:since_id = 0 OR f.id >= :since_id)
AND (:max_id = 0 OR f.id <= :max_id)
ORDER BY f.created_at DESC
LIMIT :limit|}
    ~p:
      [
        ("id", `Int (Account.ID.to_int id));
        ("limit", `Int limit);
        ("since_id", `Int (Option.fold ~none:0 ~some:Follow.ID.to_int since_id));
        ("max_id", `Int (Option.fold ~none:0 ~some:Follow.ID.to_int max_id));
      ]
  >|= List.map Account.pack

let get_notifications ~account_id ~max_id ~since_id ~limit
    (c : Sqlx.Connection.t) : Notification.t list Lwt.t =
  c#named_query
    {|
SELECT n.* FROM notifications n
WHERE n.account_id = :account_id
AND (:since_id = 0 OR n.id >= :since_id)
AND (:max_id = 0 OR n.id <= :max_id)
ORDER BY n.created_at DESC
LIMIT :limit|}
    ~p:
      [
        ("account_id", `Int (Account.ID.to_int account_id));
        ("limit", `Int limit);
        ( "since_id",
          `Int (Option.fold ~none:0 ~some:Notification.ID.to_int since_id) );
        ( "max_id",
          `Int (Option.fold ~none:0 ~some:Notification.ID.to_int max_id) );
      ]
  >|= List.map Notification.pack

let get_local_followers ~account_id c : User.t list Lwt.t =
  let%lwt ids =
    Follow.get_many ~target_account_id:account_id c
    >|= List.map (fun x -> x#account_id)
  in
  User.select ~account_id:(`In ids) c

let get_remote_followers ~account_id c : Account.t list Lwt.t =
  Follow.get_many ~target_account_id:account_id c ~preload:[ `account [] ]
  >|= List.filter_map (fun x ->
          x#account#domain |> Option.map (fun _ -> x#account))
