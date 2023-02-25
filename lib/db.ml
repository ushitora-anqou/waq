open Util
open Lwt.Infix

let global_pool = ref None

let initialize () =
  let pool = Sql.connect_pool 10 (Config.db_url ()) in
  global_pool := Some pool

let do_query f = Sql.use (Option.get !global_pool) f

let maybe_no_row e =
  match%lwt e with
  | exception Sql.NoRowFound -> Lwt.return_none
  | res -> Lwt.return_some res

let debug_drop_all_tables_in_db () =
  do_query @@ fun c ->
  Sql.execute c
    {|
-- Thanks to: https://stackoverflow.com/a/36023359
DO $$ DECLARE
    r RECORD;
BEGIN
    -- if the schema you operate on is not "current", you will want to
    -- replace current_schema() in query with 'schematodeletetablesfrom'
    -- *and* update the generate 'DROP...' accordingly.
    FOR r IN (SELECT tablename FROM pg_tables WHERE schemaname = current_schema()) LOOP
        EXECUTE 'DROP TABLE IF EXISTS ' || quote_ident(r.tablename) || ' CASCADE';
    END LOOP;
END $$|}

module AccountStat = struct
  type t = {
    id : int; [@sql.auto_increment]
    account_id : int;
    statuses_count : int;
    following_count : int;
    followers_count : int;
    created_at : Ptime.t;
    updated_at : Ptime.t;
    last_status_at : Ptime.t option;
  }
  [@@sql.table_name "account_stats"] [@@deriving sql, make]

  let increment ~account_id ?(statuses_count = 0) ?(following_count = 0)
      ?(followers_count = 0) ?last_status_at () =
    do_query @@ fun c ->
    Sql.execute c
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
          `Int account_id;
          `Int statuses_count;
          `Int following_count;
          `Int followers_count;
          `NullTimestamp last_status_at;
        ]

  let decrement ~account_id ?(statuses_count = 0) ?(following_count = 0)
      ?(followers_count = 0) =
    let statuses_count = -statuses_count in
    let following_count = -following_count in
    let followers_count = -followers_count in
    increment ~account_id ~statuses_count ~following_count ~followers_count
end

module StatusStat = struct
  type t = {
    id : int;
    status_id : int;
    replies_count : int;
    reblogs_count : int;
    favourites_count : int;
    created_at : Ptime.t;
    updated_at : Ptime.t;
  }
  [@@sql.table_name "status_stats"] [@@deriving sql, make]

  let increment ~status_id ?(replies_count = 0) ?(reblogs_count = 0)
      ?(favourites_count = 0) () =
    do_query @@ fun c ->
    Sql.execute c
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
          `Int status_id;
          `Int replies_count;
          `Int reblogs_count;
          `Int favourites_count;
        ]

  let decrement ~status_id ?(replies_count = 0) ?(reblogs_count = 0)
      ?(favourites_count = 0) () =
    let replies_count = -replies_count in
    let reblogs_count = -reblogs_count in
    let favourites_count = -favourites_count in
    increment ~status_id ~replies_count ~reblogs_count ~favourites_count ()
end

module User = struct
  type t = {
    id : int; [@sql.auto_increment]
    email : string;
    created_at : Ptime.t;
    updated_at : Ptime.t;
    encrypted_password : string;
    account_id : int;
  }
  [@@sql.table_name "users"] [@@deriving make, sql]
end

module Account = struct
  type t = {
    id : int; [@default 0] [@sql.auto_increment]
    username : string;
    domain : string option;
    private_key : string option;
    public_key : string;
    display_name : string;
    uri : string;
    url : string option;
    inbox_url : string;
    outbox_url : string;
    shared_inbox_url : string;
    followers_url : string;
    created_at : Ptime.t;
    updated_at : Ptime.t;
  }
  [@@sql.table_name "accounts"] [@@deriving make, sql]

  let is_local ~id : bool Lwt.t = get_one ~id () >|= fun a -> a.domain = None
  let is_remote ~id : bool Lwt.t = is_local ~id >|= fun b -> not b

  let update_one ~id a () =
    (* FIXME: This function should be generated by ppx *)
    assert (a.id = id);
    let query =
      [ "display_name" ]
      |> List.map (fun s -> s ^ " = :" ^ s)
      |> String.concat ", "
    in
    named_query_row
      ("UPDATE accounts SET " ^ query ^ " WHERE id = :id RETURNING *")
      a

  let preferred_inbox_url a =
    match (a.inbox_url, a.shared_inbox_url) with s, "" -> s | _, s -> s

  let preferred_inbox_urls accts =
    accts |> List.map preferred_inbox_url |> List.sort_uniq compare
end

module Status = struct
  type t = {
    id : int; [@sql.auto_increment]
    uri : string; [@default ""]
    text : string;
    created_at : Ptime.t;
    updated_at : Ptime.t;
    deleted_at : Ptime.t option;
    in_reply_to_id : int option;
    reblog_of_id : int option;
    account_id : int;
  }
  [@@sql.table_name "statuses"] [@@deriving make, sql]

  let get_one' = get_one
  let get_many' = get_many
  let get_one = get_one ~deleted_at:None
  let get_many = get_many ~deleted_at:None
  let delete () = assert false

  let save_one s =
    let%lwt s = save_one s in
    AccountStat.increment ~account_id:s.account_id ~statuses_count:1
      ~last_status_at:s.created_at ();%lwt
    s.reblog_of_id
    |> Lwt_option.iter (fun status_id ->
           StatusStat.increment ~status_id ~reblogs_count:1 ());%lwt
    s.in_reply_to_id
    |> Lwt_option.iter (fun status_id ->
           StatusStat.increment ~status_id ~replies_count:1 ());%lwt
    Lwt.return s

  let save_one_with_uri s =
    let%lwt s = save_one s in
    let%lwt self = Account.get_one ~id:s.account_id () in
    let uri = self.uri ^/ "statuses" ^/ string_of_int s.id in
    let s = { s with uri } in
    named_query_row "UPDATE statuses SET uri = :uri WHERE id = :id RETURNING *"
      s

  let get_reblogs_count (id : int) : int Lwt.t =
    do_query @@ fun c ->
    Sql.query_row c
      {|SELECT COUNT(*) FROM statuses WHERE deleted_at IS NULL AND reblog_of_id = $1|}
      ~p:[ `Int id ]
    >|= List.hd >|= snd >|= Sql.Value.expect_int

  let get_replies_count (id : int) : int Lwt.t =
    do_query @@ fun c ->
    Sql.query_row c
      {|SELECT COUNT(*) FROM statuses WHERE deleted_at IS NULL AND in_reply_to_id = $1|}
      ~p:[ `Int id ]
    >|= List.hd >|= snd >|= Sql.Value.expect_int

  let get_ancestors id : t list Lwt.t =
    query
      ~p:[ `Int id ]
      {|
WITH RECURSIVE t(id) AS (
  SELECT in_reply_to_id FROM statuses WHERE deleted_at IS NULL AND id = $1
  UNION
  SELECT in_reply_to_id FROM statuses s, t WHERE deleted_at IS NULL AND s.id = t.id
)
SELECT * FROM statuses WHERE id IN (SELECT * FROM t)|}

  let get_descendants id : t list Lwt.t =
    query
      ~p:[ `Int id ]
      {|
WITH RECURSIVE t(id) AS (
  SELECT id FROM statuses WHERE deleted_at IS NULL AND in_reply_to_id = $1
  UNION
  SELECT s.id FROM statuses s, t WHERE deleted_at IS NULL AND s.in_reply_to_id = t.id
)
SELECT * FROM statuses WHERE id IN (SELECT * FROM t)|}

  let discard_with_reblogs id : t Lwt.t =
    let time = Ptime.now () in
    let%lwt reblogs =
      query
        {|UPDATE statuses SET deleted_at = $2 WHERE reblog_of_id = $1 AND deleted_at IS NULL RETURNING *|}
        ~p:[ `Int id; `Timestamp time ]
    in
    let%lwt status =
      query_row "UPDATE statuses SET deleted_at = $2 WHERE id = $1 RETURNING *"
        ~p:[ `Int id; `Timestamp time ]
    in
    status :: reblogs
    |> Lwt_list.iter_p (fun s ->
           AccountStat.decrement ~account_id:s.account_id ~statuses_count:1 ();%lwt
           s.reblog_of_id
           |> Lwt_option.iter (fun status_id ->
                  StatusStat.decrement ~status_id ~reblogs_count:1 ());%lwt
           s.in_reply_to_id
           |> Lwt_option.iter (fun status_id ->
                  StatusStat.decrement ~status_id ~replies_count:1 ());%lwt
           Lwt.return_unit);%lwt
    Lwt.return status
end

module Follow = struct
  type t = {
    id : int; [@sql.auto_increment]
    created_at : Ptime.t;
    updated_at : Ptime.t;
    account_id : int;
    target_account_id : int;
    uri : string;
  }
  [@@sql.table_name "follows"] [@@deriving make, sql]

  let save_one f =
    let%lwt f = save_one f in
    AccountStat.increment ~account_id:f.account_id ~following_count:1 ();%lwt
    AccountStat.increment ~account_id:f.target_account_id ~followers_count:1 ();%lwt
    Lwt.return f

  let delete f =
    delete ~id:f.id ();%lwt
    AccountStat.decrement ~account_id:f.account_id ~following_count:1 ();%lwt
    AccountStat.decrement ~account_id:f.target_account_id ~followers_count:1 ();%lwt
    Lwt.return_unit

  let does_follow ~account_id ~target_account_id : bool Lwt.t =
    match%lwt get_one ~account_id ~target_account_id () with
    | _ -> Lwt.return_true
    | exception Sql.NoRowFound -> Lwt.return_false
end

module FollowRequest = struct
  type t = {
    id : int; [@sql.auto_increment]
    created_at : Ptime.t;
    updated_at : Ptime.t;
    account_id : int;
    target_account_id : int;
    uri : string;
  }
  [@@table_name "follow_requests"] [@@deriving make, sql]
end

module OAuthApplication = struct
  type t = {
    id : int; [@sql.auto_increment]
    name : string;
    uid : string;
    secret : string;
    redirect_uri : string;
    scopes : string;
    created_at : Ptime.t option;
    updated_at : Ptime.t option;
  }
  [@@sql.table_name "oauth_applications"] [@@deriving make, sql]
end

module OAuthAccessGrant = struct
  type t = {
    id : int; [@sql.auto_increment]
    token : string;
    expires_in : int;
    redirect_uri : string;
    created_at : Ptime.t;
    scopes : string option;
    application_id : int option;
    resource_owner_id : int option;
  }
  [@@sql.table_name "oauth_access_grants"] [@@deriving make, sql]
end

module OAuthAccessToken = struct
  type t = {
    id : int; [@sql.auto_increment]
    token : string;
    created_at : Ptime.t;
    scopes : string option;
    application_id : int option;
    resource_owner_id : int option;
  }
  [@@sql.table_name "oauth_access_tokens"] [@@deriving make, sql]
end

module Favourite = struct
  type t = {
    id : int; [@sql.auto_increment]
    created_at : Ptime.t;
    updated_at : Ptime.t;
    account_id : int;
    status_id : int;
  }
  [@@sql.table_name "favourites"] [@@deriving make, sql]

  let save_one f =
    let%lwt f = save_one f in
    StatusStat.increment ~status_id:f.status_id ~favourites_count:1 ();%lwt
    Lwt.return f

  let delete f =
    delete ~id:f.id ();%lwt
    StatusStat.decrement ~status_id:f.status_id ~favourites_count:1 ()

  let get_favourites_count ~status_id =
    do_query @@ fun c ->
    Sql.query_row c {|SELECT COUNT(*) FROM favourites WHERE status_id = $1|}
      ~p:[ `Int status_id ]
    >|= List.hd >|= snd >|= Sql.Value.expect_int
end

module Notification = struct
  type activity_type_t = [ `Status | `Favourite | `Follow ]

  let string_of_activity_type_t : activity_type_t -> string = function
    | `Status -> "Status"
    | `Favourite -> "Favourite"
    | `Follow -> "Follow"

  let activity_type_t_of_string : string -> activity_type_t = function
    | "Status" -> `Status
    | "Favourite" -> `Favourite
    | "Follow" -> `Follow
    | _ -> failwith "activity_type_t_of_string: invalid input"

  type typ_t = [ `reblog | `favourite | `follow ]

  let string_of_typ_t : typ_t -> string = function
    | `reblog -> "reblog"
    | `favourite -> "favourite"
    | `follow -> "follow"

  let typ_t_of_string : string -> typ_t = function
    | "reblog" -> `reblog
    | "favourite" -> `favourite
    | "follow" -> `follow
    | _ -> failwith "type_t_of_string: invalid input"

  type t = {
    id : int; [@sql.auto_increment]
    activity_id : int;
    activity_type : activity_type_t;
        [@sql.column_encoding
          string_of_activity_type_t, activity_type_t_of_string]
    created_at : Ptime.t;
    updated_at : Ptime.t;
    account_id : int;
    from_account_id : int;
    typ : typ_t option;
        [@sql.column_name "type"]
        [@sql.column_encoding string_of_typ_t, typ_t_of_string]
  }
  [@@sql.table_name "notifications"] [@@deriving make, sql]
end

let home_timeline ~id ~limit ~max_id ~since_id : Status.t list Lwt.t =
  Status.query
    {|
SELECT * FROM statuses
WHERE
  deleted_at IS NULL AND
  ( account_id = $1 OR
    account_id IN (SELECT target_account_id FROM follows WHERE account_id = $1) ) AND
  ( $3 = 0 OR id >= $3 ) AND ( $4 = 0 OR id <= $4 )
ORDER BY created_at DESC LIMIT $2|}
    ~p:
      [
        `Int id;
        `Int limit;
        `Int (Option.value ~default:0 since_id);
        `Int (Option.value ~default:0 max_id);
      ]

let account_statuses ~id ~limit ~max_id ~since_id ~exclude_replies :
    Status.t list Lwt.t =
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
  do_query @@ fun c ->
  Sql.named_query c sql
    ~p:
      [
        ("id", `Int id);
        ("limit", `Int limit);
        ("since_id", `Int (Option.value ~default:0 since_id));
        ("max_id", `Int (Option.value ~default:0 max_id));
      ]
  >|= List.map Status.pack

let get_favourited_by ~status_id =
  Account.query
    {|
SELECT a.* FROM accounts a
INNER JOIN favourites f ON a.id = f.account_id
WHERE f.status_id = $1
  |}
    ~p:[ `Int status_id ]

let count_statuses ~account_id : int Lwt.t =
  do_query @@ fun c ->
  Sql.query_row c
    {|SELECT COUNT(*) FROM statuses WHERE deleted_at IS NULL AND account_id = $1|}
    ~p:[ `Int account_id ]
  >|= List.hd >|= snd >|= Sql.Value.expect_int

let get_last_status_at ~account_id : Ptime.t option Lwt.t =
  Status.query
    {|SELECT * FROM statuses WHERE deleted_at IS NULL AND account_id = $1 ORDER BY created_at DESC LIMIT 1 |}
    ~p:[ `Int account_id ]
  >|= function
  | [ s ] -> Some s.created_at
  | _ -> None

let count_followers ~account_id : int Lwt.t =
  do_query @@ fun c ->
  Sql.query_row c {|SELECT COUNT(*) FROM follows WHERE target_account_id = $1|}
    ~p:[ `Int account_id ]
  >|= List.hd >|= snd >|= Sql.Value.expect_int

let count_following ~account_id : int Lwt.t =
  do_query @@ fun c ->
  Sql.query_row c {|SELECT COUNT(*) FROM follows WHERE account_id = $1|}
    ~p:[ `Int account_id ]
  >|= List.hd >|= snd >|= Sql.Value.expect_int

let get_followers ~id ~self_id:_ ~max_id ~since_id ~limit : Account.t list Lwt.t
    =
  do_query @@ fun c ->
  Sql.named_query c
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
        ("id", `Int id);
        ("limit", `Int limit);
        ("since_id", `Int (Option.value ~default:0 since_id));
        ("max_id", `Int (Option.value ~default:0 max_id));
      ]
  >|= List.map Account.pack

let get_following ~id ~self_id:_ ~max_id ~since_id ~limit : Account.t list Lwt.t
    =
  do_query @@ fun c ->
  Sql.named_query c
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
        ("id", `Int id);
        ("limit", `Int limit);
        ("since_id", `Int (Option.value ~default:0 since_id));
        ("max_id", `Int (Option.value ~default:0 max_id));
      ]
  >|= List.map Account.pack

let get_notifications ~account_id ~max_id ~since_id ~limit :
    Notification.t list Lwt.t =
  do_query @@ fun c ->
  Sql.named_query c
    {|
SELECT n.* FROM notifications n
WHERE n.account_id = :account_id
AND (:since_id = 0 OR n.id >= :since_id)
AND (:max_id = 0 OR n.id <= :max_id)
ORDER BY n.created_at DESC
LIMIT :limit|}
    ~p:
      [
        ("account_id", `Int account_id);
        ("limit", `Int limit);
        ("since_id", `Int (Option.value ~default:0 since_id));
        ("max_id", `Int (Option.value ~default:0 max_id));
      ]
  >|= List.map Notification.pack

let get_local_followers ~account_id : User.t list Lwt.t =
  User.query
    {|
SELECT u.* FROM users u
WHERE u.id IN (
  SELECT a.id FROM accounts a
  INNER JOIN follows f ON a.id = f.account_id
  WHERE f.target_account_id = $1)|}
    ~p:[ `Int account_id ]

let get_remote_followers ~account_id : Account.t list Lwt.t =
  Account.query
    {|
SELECT a.* FROM accounts a
INNER JOIN follows f ON a.id = f.account_id
WHERE f.target_account_id = $1 AND a.domain IS NOT NULL|}
    ~p:[ `Int account_id ]

let register_user ~username ~display_name ~email ~password =
  let now = Ptime.now () in
  let created_at, updated_at = (now, now) in
  let private_key, public_key = Httpq.Signature.generate_keypair () in
  let public_key = Httpq.Signature.encode_public_key public_key in
  let private_key = Httpq.Signature.encode_private_key private_key in
  let uri = Config.url [ "users"; username ] in
  let inbox_url = uri ^/ "inbox" in
  let outbox_url = uri ^/ "outbox" in
  let followers_url = uri ^/ "followers" in
  let shared_inbox_url = Config.url [ "inbox" ] in
  let encrypted_password = Bcrypt.(hash password |> string_of_hash) in
  let%lwt a =
    Account.(
      make ~username ~public_key ~private_key ~display_name ~uri ~inbox_url
        ~outbox_url ~followers_url ~created_at ~updated_at ~shared_inbox_url ()
      |> save_one)
  in
  let%lwt u =
    let created_at, updated_at = (now, now) in
    User.(
      make ~id:0 ~email ~created_at ~updated_at ~account_id:a.id
        ~encrypted_password
      |> save_one)
  in
  Lwt.return (a, u)
