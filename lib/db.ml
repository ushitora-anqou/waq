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

module Migration = struct
  module type S = sig
    val up : Sql.connection -> unit Lwt.t
    val down : Sql.connection -> unit Lwt.t
  end

  module M20221230_220000_Big_Bang : S = struct
    let up c =
      Sql.execute c
        {|
CREATE TABLE accounts (
  id SERIAL PRIMARY KEY,
  username TEXT NOT NULL,
  domain TEXT,
  private_key TEXT,
  public_key TEXT NOT NULL,
  display_name TEXT NOT NULL,
  uri TEXT NOT NULL,
  url TEXT,
  inbox_url TEXT NOT NULL,
  followers_url TEXT NOT NULL,
  created_at TIMESTAMP WITHOUT TIME ZONE NOT NULL,
  updated_at TIMESTAMP WITHOUT TIME ZONE NOT NULL,

  UNIQUE (username, domain)
)|};%lwt
      Sql.execute c
        {|
CREATE TABLE users (
  id SERIAL PRIMARY KEY,
  email TEXT NOT NULL,
  created_at TIMESTAMP WITHOUT TIME ZONE NOT NULL,
  updated_at TIMESTAMP WITHOUT TIME ZONE NOT NULL,
  account_id BIGINT NOT NULL,

  FOREIGN KEY (account_id) REFERENCES accounts (id) ON UPDATE CASCADE ON DELETE CASCADE
)|};%lwt
      Sql.execute c
        {|
CREATE TABLE statuses (
  id SERIAL PRIMARY KEY,
  uri TEXT NOT NULL,
  text TEXT NOT NULL,
  created_at TIMESTAMP WITHOUT TIME ZONE NOT NULL,
  updated_at TIMESTAMP WITHOUT TIME ZONE NOT NULL,
  account_id BIGINT NOT NULL,

  FOREIGN KEY (account_id) REFERENCES accounts (id) ON UPDATE CASCADE ON DELETE CASCADE
)|};%lwt
      Sql.execute c
        {|
CREATE TABLE follows (
  id SERIAL PRIMARY KEY,
  created_at TIMESTAMP WITHOUT TIME ZONE NOT NULL,
  updated_at TIMESTAMP WITHOUT TIME ZONE NOT NULL,
  account_id BIGINT,
  target_account_id BIGINT,
  uri TEXT,

  FOREIGN KEY (account_id) REFERENCES accounts (id) ON UPDATE CASCADE ON DELETE CASCADE,
  FOREIGN KEY (target_account_id) REFERENCES accounts (id) ON UPDATE CASCADE ON DELETE CASCADE,
  UNIQUE (account_id, target_account_id)
)|};%lwt
      Sql.execute c
        {|
CREATE TABLE follow_requests (
  id SERIAL PRIMARY KEY,
  created_at TIMESTAMP WITHOUT TIME ZONE NOT NULL,
  updated_at TIMESTAMP WITHOUT TIME ZONE NOT NULL,
  account_id BIGINT,
  target_account_id BIGINT,
  uri TEXT,

  FOREIGN KEY (account_id) REFERENCES accounts (id) ON UPDATE CASCADE ON DELETE CASCADE,
  FOREIGN KEY (target_account_id) REFERENCES accounts (id) ON UPDATE CASCADE ON DELETE CASCADE,
  UNIQUE (account_id, target_account_id)
)|}

    let down c =
      Sql.execute c {|DROP TABLE follow_requests|};%lwt
      Sql.execute c {|DROP TABLE follows|};%lwt
      Sql.execute c {|DROP TABLE statuses|};%lwt
      Sql.execute c {|DROP TABLE users|};%lwt
      Sql.execute c {|DROP TABLE accounts|}
  end

  let all : (int * (module S)) list =
    [ (20221230220000, (module M20221230_220000_Big_Bang)) ]

  let process kind =
    (* FIXME: Use transaction *)
    do_query @@ fun c ->
    all
    |> Lwt_list.iter_s (fun (id, (module M : S)) ->
           match kind with
           | `Migrate ->
               Log.info (fun m -> m "Migrate %d" id);
               M.up c
           | `Rollback ->
               Log.info (fun m -> m "Rollback %d" id);
               M.down c)
end

let migrate () = Migration.process `Migrate
let rollback () = Migration.process `Rollback
