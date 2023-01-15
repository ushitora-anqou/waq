open Sql
module Pg = Make (PgDriver)
open Util
open Lwt.Infix

let global_pool = ref None

let initialize () =
  let pool = Pg.connect_pool 10 (Config.db_url ()) in
  global_pool := Some pool

let do_query f = Pg.use (Option.get !global_pool) f

let maybe_no_row e =
  match%lwt e with
  | exception Sql.NoRowFound -> Lwt.return_none
  | res -> Lwt.return_some res

type user = {
  id : int;
  email : string;
  created_at : Ptime.t;
  updated_at : Ptime.t;
  account_id : int;
}
[@@deriving make]

(* FIXME: pack_user (or get_uesr) should be auto-generated by PPX *)
let pack_user (l : single_query_result) =
  let id = l |> List.assoc "id" |> Value.expect_int in
  let email = l |> List.assoc "email" |> Value.expect_string in
  let created_at = l |> List.assoc "created_at" |> Value.expect_timestamp in
  let updated_at = l |> List.assoc "updated_at" |> Value.expect_timestamp in
  let account_id = l |> List.assoc "account_id" |> Value.expect_int in
  { id; email; created_at; updated_at; account_id }

(* FIXME: unpack_user (or insert_user) should be auto-generated by PPX *)
let unpack_user (u : user) =
  [
    ("id", `Int u.id);
    ("email", `String u.email);
    ("created_at", `Timestamp u.created_at);
    ("updated_at", `Timestamp u.updated_at);
    ("account_id", `Int u.account_id);
  ]

let get_user ~by =
  do_query @@ fun c ->
  pack_user
  =|<
  match by with
  | `id id -> Pg.query_row c "SELECT * FROM users WHERE id = $1" ~p:[ `Int id ]
  | `username n ->
      Pg.query_row c
        {|SELECT * FROM users
          INNER JOIN accounts ON users.account_id = accounts.id
          WHERE accounts.username = $1|}
        ~p:[ `String n ]

let insert_user (u : user) : user Lwt.t =
  do_query @@ fun c ->
  Pg.named_query_row c
    {|INSERT INTO users (email, created_at, updated_at, account_id)
      VALUES (:email, :created_at, :updated_at, :account_id)
      RETURNING *|}
    ~p:(unpack_user u)
  >|= pack_user

type account = {
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
[@@deriving make]

let pack_account (l : single_query_result) =
  let id = l |> List.assoc "id" |> Value.expect_int in
  let username = l |> List.assoc "username" |> Value.expect_string in
  let domain = l |> List.assoc "domain" |> Value.expect_string_opt in
  let private_key = l |> List.assoc "private_key" |> Value.expect_string_opt in
  let public_key = l |> List.assoc "public_key" |> Value.expect_string in
  let display_name = l |> List.assoc "display_name" |> Value.expect_string in
  let uri = l |> List.assoc "uri" |> Value.expect_string in
  let url = l |> List.assoc "url" |> Value.expect_string_opt in
  let inbox_url = l |> List.assoc "inbox_url" |> Value.expect_string in
  let followers_url = l |> List.assoc "followers_url" |> Value.expect_string in
  let created_at = l |> List.assoc "created_at" |> Value.expect_timestamp in
  let updated_at = l |> List.assoc "updated_at" |> Value.expect_timestamp in
  {
    id;
    username;
    domain;
    private_key;
    public_key;
    display_name;
    uri;
    url;
    inbox_url;
    followers_url;
    created_at;
    updated_at;
  }

let unpack_account (a : account) =
  let sopt s = s |> Option.fold ~none:`Null ~some:(fun s -> `String s) in
  [
    ("id", `Int a.id);
    ("username", `String a.username);
    ("domain", sopt a.domain);
    ("private_key", sopt a.private_key);
    ("public_key", `String a.public_key);
    ("display_name", `String a.display_name);
    ("uri", `String a.uri);
    ("url", sopt a.url);
    ("inbox_url", `String a.inbox_url);
    ("followers_url", `String a.followers_url);
    ("created_at", `Timestamp a.created_at);
    ("updated_at", `Timestamp a.updated_at);
  ]

let get_account ~by =
  do_query @@ fun c ->
  pack_account
  =|<
  match by with
  | `id id ->
      Pg.query_row c "SELECT * FROM accounts WHERE id = $1" ~p:[ `Int id ]
  | `domain_username (domain, username) ->
      Pg.query_row c
        {|SELECT * FROM accounts
          WHERE COALESCE(domain, '') = $1 AND username = $2|}
        ~p:[ `String domain; `String username ]
  | `uri u ->
      Pg.query_row c "SELECT * FROM accounts WHERE uri = $1" ~p:[ `String u ]

let insert_account (a : account) =
  do_query @@ fun c ->
  Pg.named_query_row c
    {|
INSERT INTO accounts (
  username, domain, private_key, public_key, display_name, uri, url, inbox_url, followers_url, created_at, updated_at)
VALUES (
  :username, :domain, :private_key, :public_key, :display_name, :uri, :url, :inbox_url, :followers_url, :created_at, :updated_at)
RETURNING *|}
    ~p:(unpack_account a)
  >|= pack_account

type status = {
  id : int;
  uri : string;
  text : string;
  created_at : Ptime.t;
  updated_at : Ptime.t;
  account_id : int;
}
[@@deriving make]

let pack_status (l : single_query_result) =
  let id = l |> List.assoc "id" |> Value.expect_int in
  let uri = l |> List.assoc "uri" |> Value.expect_string in
  let text = l |> List.assoc "text" |> Value.expect_string in
  let created_at = l |> List.assoc "created_at" |> Value.expect_timestamp in
  let updated_at = l |> List.assoc "updated_at" |> Value.expect_timestamp in
  let account_id = l |> List.assoc "account_id" |> Value.expect_int in
  { id; uri; text; created_at; updated_at; account_id }

let unpack_status (s : status) =
  [
    ("id", `Int s.id);
    ("uri", `String s.uri);
    ("text", `String s.text);
    ("created_at", `Timestamp s.created_at);
    ("updated_at", `Timestamp s.updated_at);
    ("account_id", `Int s.account_id);
  ]

let insert_status (s : status) =
  do_query @@ fun c ->
  Pg.named_query_row c
    {|
INSERT INTO statuses (uri, text, created_at, updated_at, account_id)
VALUES (:uri, :text, :created_at, :updated_at, :account_id)
RETURNING *|}
    ~p:(unpack_status s)
  >|= pack_status

let update_status_uri (s : status) =
  do_query @@ fun c ->
  Pg.named_query_row c
    "UPDATE statuses SET uri = :uri WHERE id = :id RETURNING *"
    ~p:(unpack_status s)
  >|= pack_status

type follow = {
  id : int;
  created_at : Ptime.t;
  updated_at : Ptime.t;
  account_id : int;
  target_account_id : int;
  uri : string;
}
[@@deriving make]

let pack_follow (l : single_query_result) =
  let id = l |> List.assoc "id" |> Value.expect_int in
  let created_at = l |> List.assoc "created_at" |> Value.expect_timestamp in
  let updated_at = l |> List.assoc "updated_at" |> Value.expect_timestamp in
  let account_id = l |> List.assoc "account_id" |> Value.expect_int in
  let target_account_id =
    l |> List.assoc "target_account_id" |> Value.expect_int
  in
  let uri = l |> List.assoc "uri" |> Value.expect_string in
  { id; created_at; updated_at; account_id; target_account_id; uri }

let unpack_follow (f : follow) =
  [
    ("id", `Int f.id);
    ("created_at", `Timestamp f.created_at);
    ("updated_at", `Timestamp f.updated_at);
    ("account_id", `Int f.account_id);
    ("target_account_id", `Int f.target_account_id);
    ("uri", `String f.uri);
  ]

let get_follow ~by =
  do_query @@ fun c ->
  pack_follow
  =|<
  match by with
  | `accounts (account_id, target_account_id) ->
      Pg.query_row c
        "SELECT * FROM follows WHERE account_id = $1 AND target_account_id = $2"
        ~p:[ `Int account_id; `Int target_account_id ]

let get_follows ~by =
  do_query @@ fun c ->
  match by with
  | `target_account_id id ->
      Pg.query c "SELECT * FROM follows WHERE target_account_id = $1"
        ~p:[ `Int id ]
      >|= List.map pack_follow

let insert_follow (f : follow) =
  do_query @@ fun c ->
  Pg.named_query_row c
    {|
INSERT INTO follows (created_at, updated_at, account_id, target_account_id, uri)
VALUES (:created_at, :updated_at, :account_id, :target_account_id, :uri)
RETURNING *|}
    ~p:(unpack_follow f)
  >|= pack_follow

let delete_follow ~by =
  do_query @@ fun c ->
  match by with
  | `uri u -> Pg.execute c "DELETE FROM follows WHERE uri = $1" ~p:[ `String u ]
  | `accounts (id1, id2) ->
      Pg.execute c
        "DELETE FROM follows WHERE account_id = $1 AND target_account_id = $2"
        ~p:[ `Int id1; `Int id2 ]

type follow_request = {
  id : int;
  created_at : Ptime.t;
  updated_at : Ptime.t;
  account_id : int;
  target_account_id : int;
  uri : string;
}
[@@deriving make]

let pack_follow_request (l : single_query_result) =
  let id = l |> List.assoc "id" |> Value.expect_int in
  let created_at = l |> List.assoc "created_at" |> Value.expect_timestamp in
  let updated_at = l |> List.assoc "updated_at" |> Value.expect_timestamp in
  let account_id = l |> List.assoc "account_id" |> Value.expect_int in
  let target_account_id =
    l |> List.assoc "target_account_id" |> Value.expect_int
  in
  let uri = l |> List.assoc "uri" |> Value.expect_string in
  { id; created_at; updated_at; account_id; target_account_id; uri }

let unpack_follow_request (f : follow_request) =
  [
    ("id", `Int f.id);
    ("created_at", `Timestamp f.created_at);
    ("updated_at", `Timestamp f.updated_at);
    ("account_id", `Int f.account_id);
    ("target_account_id", `Int f.target_account_id);
    ("uri", `String f.uri);
  ]

let get_follow_request ~by =
  do_query @@ fun c ->
  pack_follow_request
  =|<
  match by with
  | `accounts (account_id, target_account_id) ->
      Pg.query_row c
        "SELECT * FROM follow_requests WHERE account_id = $1 AND \
         target_account_id = $2"
        ~p:[ `Int account_id; `Int target_account_id ]
  | `uri uri ->
      Pg.query_row c "SELECT * FROM follow_requests WHERE uri = $1"
        ~p:[ `String uri ]

let insert_follow_request (f : follow_request) =
  do_query @@ fun c ->
  Pg.named_query_row c
    {|
INSERT INTO follow_requests (created_at, updated_at, account_id, target_account_id, uri)
VALUES (:created_at, :updated_at, :account_id, :target_account_id, :uri)
RETURNING *|}
    ~p:(unpack_follow_request f)

let delete_follow_request ~by =
  do_query @@ fun c ->
  match by with
  | `id i ->
      Pg.execute c "DELETE FROM follow_requests WHERE id = $1" ~p:[ `Int i ]
  | `uri u ->
      Pg.execute c "DELETE FROM follow_requests WHERE uri = $1" ~p:[ `String u ]

module Migration = struct
  module type S = sig
    val up : Pg.connection -> unit Lwt.t
    val down : Pg.connection -> unit Lwt.t
  end

  module M20221230_220000_Big_Bang : S = struct
    let up c =
      Pg.execute c
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
      Pg.execute c
        {|
CREATE TABLE users (
  id SERIAL PRIMARY KEY,
  email TEXT NOT NULL,
  created_at TIMESTAMP WITHOUT TIME ZONE NOT NULL,
  updated_at TIMESTAMP WITHOUT TIME ZONE NOT NULL,
  account_id BIGINT NOT NULL,

  FOREIGN KEY (account_id) REFERENCES accounts (id) ON UPDATE CASCADE ON DELETE CASCADE
)|};%lwt
      Pg.execute c
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
      Pg.execute c
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
      Pg.execute c
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
      Pg.execute c {|DROP TABLE follow_requests|};%lwt
      Pg.execute c {|DROP TABLE follows|};%lwt
      Pg.execute c {|DROP TABLE statuses|};%lwt
      Pg.execute c {|DROP TABLE users|};%lwt
      Pg.execute c {|DROP TABLE accounts|}
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
