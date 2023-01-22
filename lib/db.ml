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
    id : int;
    email : string;
    created_at : Ptime.t;
    updated_at : Ptime.t;
    account_id : int;
  }
  [@@deriving make, sql]

  let get ~by =
    do_query @@ fun c ->
    match by with
    | `id id -> query_row c "SELECT * FROM users WHERE id = $1" ~p:[ `Int id ]
    | `username n ->
        query_row c
          {|
SELECT * FROM users
INNER JOIN accounts ON users.account_id = accounts.id
WHERE accounts.username = $1|}
          ~p:[ `String n ]

  let insert u =
    do_query @@ fun c ->
    named_query_row c
      {|
INSERT INTO users (email, created_at, updated_at, account_id)
VALUES (:email, :created_at, :updated_at, :account_id)
RETURNING *|}
      u
end

module Account = struct
  type t = {
    id : int; [@default 0]
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
  [@@deriving make, sql]

  let get ~by =
    do_query @@ fun c ->
    match by with
    | `id id ->
        query_row c "SELECT * FROM accounts WHERE id = $1" ~p:[ `Int id ]
    | `domain_username (domain, username) ->
        query_row c
          {|SELECT * FROM accounts WHERE domain IS NOT DISTINCT FROM $1 AND username = $2|}
          ~p:[ `NullString domain; `String username ]
    | `uri u ->
        query_row c "SELECT * FROM accounts WHERE uri = $1" ~p:[ `String u ]

  let insert a =
    do_query @@ fun c ->
    named_query_row c
      {|
INSERT INTO accounts (
  username, domain, private_key, public_key, display_name, uri, url, inbox_url, followers_url, created_at, updated_at)
VALUES (
  :username, :domain, :private_key, :public_key, :display_name, :uri, :url, :inbox_url, :followers_url, :created_at, :updated_at)
RETURNING *|}
      a
end

module Status = struct
  type t = {
    id : int;
    uri : string;
    text : string;
    created_at : Ptime.t;
    updated_at : Ptime.t;
    account_id : int;
  }
  [@@deriving make, sql]

  let insert s =
    do_query @@ fun c ->
    named_query_row c
      {|
INSERT INTO statuses (uri, text, created_at, updated_at, account_id)
VALUES (:uri, :text, :created_at, :updated_at, :account_id)
RETURNING *|}
      s

  let update_uri s =
    do_query @@ fun c ->
    named_query_row c
      "UPDATE statuses SET uri = :uri WHERE id = :id RETURNING *" s
end

module Follow = struct
  type t = {
    id : int;
    created_at : Ptime.t;
    updated_at : Ptime.t;
    account_id : int;
    target_account_id : int;
    uri : string;
  }
  [@@deriving make, sql]

  let get ~by =
    do_query @@ fun c ->
    match by with
    | `accounts (account_id, target_account_id) ->
        query_row c
          {|
SELECT * FROM follows
WHERE account_id = $1 AND target_account_id = $2|}
          ~p:[ `Int account_id; `Int target_account_id ]

  let get_many ~by =
    do_query @@ fun c ->
    match by with
    | `target_account_id id ->
        query c "SELECT * FROM follows WHERE target_account_id = $1"
          ~p:[ `Int id ]

  let insert f =
    do_query @@ fun c ->
    named_query_row c
      {|
INSERT INTO follows (created_at, updated_at, account_id, target_account_id, uri)
VALUES (:created_at, :updated_at, :account_id, :target_account_id, :uri)
RETURNING *|}
      f

  let delete ~by =
    do_query @@ fun c ->
    match by with
    | `uri u ->
        Sql.execute c "DELETE FROM follows WHERE uri = $1" ~p:[ `String u ]
    | `accounts (id1, id2) ->
        Sql.execute c
          "DELETE FROM follows WHERE account_id = $1 AND target_account_id = $2"
          ~p:[ `Int id1; `Int id2 ]
end

module FollowRequest = struct
  type t = {
    id : int;
    created_at : Ptime.t;
    updated_at : Ptime.t;
    account_id : int;
    target_account_id : int;
    uri : string;
  }
  [@@deriving make, sql]

  let get ~by =
    do_query @@ fun c ->
    match by with
    | `accounts (account_id, target_account_id) ->
        query_row c
          {|
SELECT * FROM follow_requests
WHERE account_id = $1 AND target_account_id = $2|}
          ~p:[ `Int account_id; `Int target_account_id ]
    | `uri uri ->
        query_row c "SELECT * FROM follow_requests WHERE uri = $1"
          ~p:[ `String uri ]

  let insert f =
    do_query @@ fun c ->
    named_query_row c
      {|
INSERT INTO follow_requests (created_at, updated_at, account_id, target_account_id, uri)
VALUES (:created_at, :updated_at, :account_id, :target_account_id, :uri)
RETURNING *|}
      f

  let delete ~by =
    do_query @@ fun c ->
    match by with
    | `id i ->
        Sql.execute c "DELETE FROM follow_requests WHERE id = $1" ~p:[ `Int i ]
    | `uri u ->
        Sql.execute c "DELETE FROM follow_requests WHERE uri = $1"
          ~p:[ `String u ]
end

module OAuthApplication = struct
  type t = {
    id : int;
    name : string;
    uid : string;
    secret : string;
    redirect_uri : string;
    scopes : string;
    created_at : Ptime.t option;
    updated_at : Ptime.t option;
  }
  [@@deriving make, sql]

  let insert o =
    do_query @@ fun c ->
    named_query_row c
      {|
INSERT INTO oauth_applications (
  name, uid, secret, redirect_uri, scopes, created_at, updated_at )
VALUES (
  :name, :uid, :secret, :redirect_uri, :scopes, :created_at, :updated_at )
RETURNING *
|}
      o

  let get ~by =
    do_query @@ fun c ->
    match by with
    | `uid uid ->
        query_row c {|SELECT * FROM oauth_applications WHERE uid = $1|}
          ~p:[ `String uid ]
end

module OAuthAccessGrant = struct
  type t = {
    id : int;
    token : string;
    expires_in : int;
    redirect_uri : string;
    created_at : Ptime.t;
    scopes : string option;
    application_id : int option;
    resource_owner_id : int option;
  }
  [@@deriving make, sql]

  let get ~by =
    do_query @@ fun c ->
    match by with
    | `token token ->
        query_row c {|
SELECT * FROM oauth_access_grants
WHERE token = $1|}
          ~p:[ `String token ]

  let insert o =
    do_query @@ fun c ->
    named_query_row c
      {|
INSERT INTO oauth_access_grants (
  token, expires_in, redirect_uri, created_at, scopes, application_id, resource_owner_id )
VALUES (
  :token, :expires_in, :redirect_uri, :created_at, :scopes, :application_id, :resource_owner_id )
RETURNING *|}
      o
end

module OAuthAccessToken = struct
  type t = {
    id : int;
    token : string;
    created_at : Ptime.t;
    scopes : string option;
    application_id : int option;
    resource_owner_id : int option;
  }
  [@@deriving make, sql]

  let get ~by =
    do_query @@ fun c ->
    match by with
    | `token token ->
        query_row c {|
SELECT * FROM oauth_access_tokens
WHERE token = $1|}
          ~p:[ `String token ]

  let insert o =
    do_query @@ fun c ->
    named_query_row c
      {|
INSERT INTO oauth_access_tokens (
  token, created_at, scopes, application_id, resource_owner_id )
VALUES (
  :token, :created_at, :scopes, :application_id, :resource_owner_id )
RETURNING *|}
      o
end

let home_timeline ~id ~limit ~max_id ~since_id : Status.t list Lwt.t =
  do_query @@ fun c ->
  Status.query c
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
