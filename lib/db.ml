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

module User = struct
  type t = {
    id : int; [@sql.auto_increment]
    email : string;
    created_at : Ptime.t;
    updated_at : Ptime.t;
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
    followers_url : string;
    created_at : Ptime.t;
    updated_at : Ptime.t;
  }
  [@@sql.table_name "accounts"] [@@deriving make, sql]

  let is_local ~id : bool Lwt.t = get_one ~id () >|= fun a -> a.domain = None
  let is_remote ~id : bool Lwt.t = is_local ~id >|= fun b -> not b
end

module Status = struct
  type t = {
    id : int; [@sql.auto_increment]
    uri : string; [@default ""]
    text : string;
    created_at : Ptime.t;
    updated_at : Ptime.t;
    in_reply_to_id : int option;
    reblog_of_id : int option;
    account_id : int;
  }
  [@@sql.table_name "statuses"] [@@deriving make, sql]

  let save_one_with_uri s =
    let%lwt s = save_one s in
    let%lwt self = Account.get_one ~id:s.account_id () in
    let uri = self.uri ^/ "statuses" ^/ string_of_int s.id in
    let s = { s with uri } in
    named_query_row "UPDATE statuses SET uri = :uri WHERE id = :id RETURNING *"
      s

  let get_reblogs_count (id : int) : int Lwt.t =
    do_query @@ fun c ->
    Sql.query_row c "SELECT COUNT(*) FROM statuses WHERE reblog_of_id = $1"
      ~p:[ `Int id ]
    >|= List.hd >|= snd >|= Sql.Value.expect_int

  let get_replies_count (id : int) : int Lwt.t =
    do_query @@ fun c ->
    Sql.query_row c "SELECT COUNT(*) FROM statuses WHERE in_reply_to_id = $1"
      ~p:[ `Int id ]
    >|= List.hd >|= snd >|= Sql.Value.expect_int

  let get_ancestors id : t list Lwt.t =
    query
      ~p:[ `Int id ]
      {|
WITH RECURSIVE t(id) AS (
  SELECT in_reply_to_id FROM statuses WHERE id = $1
  UNION
  SELECT in_reply_to_id FROM statuses s, t WHERE s.id = t.id
)
SELECT * FROM statuses WHERE id IN (SELECT * FROM t)|}

  let get_descendants id : t list Lwt.t =
    query
      ~p:[ `Int id ]
      {|
WITH RECURSIVE t(id) AS (
  SELECT id FROM statuses WHERE in_reply_to_id = $1
  UNION
  SELECT s.id FROM statuses s, t WHERE s.in_reply_to_id = t.id
)
SELECT * FROM statuses WHERE id IN (SELECT * FROM t)|}
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
end

let home_timeline ~id ~limit ~max_id ~since_id : Status.t list Lwt.t =
  Status.query
    {|
SELECT * FROM statuses
WHERE
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
  Sql.query_row c {|SELECT COUNT(*) FROM statuses WHERE account_id = $1|}
    ~p:[ `Int account_id ]
  >|= List.hd >|= snd >|= Sql.Value.expect_int

let get_last_status_at ~account_id : Ptime.t option Lwt.t =
  Status.query
    {|SELECT * FROM statuses WHERE account_id = $1 ORDER BY created_at DESC LIMIT 1 |}
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
