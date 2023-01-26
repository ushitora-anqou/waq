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

  let get_one_by_domain_and_username ~domain ~username =
    get_one
      ~where:"domain IS NOT DISTINCT FROM :domain AND username = :username"
      ~p:[ ("domain", `NullString domain); ("username", `String username) ]
      ()
end

module Status = struct
  type t = {
    id : int; [@sql.auto_increment]
    uri : string;
    text : string;
    created_at : Ptime.t;
    updated_at : Ptime.t;
    account_id : int;
  }
  [@@sql.table_name "statuses"] [@@deriving make, sql]

  let update_uri s =
    named_query_row "UPDATE statuses SET uri = :uri WHERE id = :id RETURNING *"
      s
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
