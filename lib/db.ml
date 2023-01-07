module C = Config

type user = {
  id : int;
  email : string;
  created_at : Ptime.t;
  updated_at : Ptime.t;
  account_id : int;
}
[@@deriving make]

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

type status = {
  id : int;
  uri : string;
  text : string;
  created_at : Ptime.t;
  updated_at : Ptime.t;
  account_id : int;
}

let make_status ~id ~uri ~text ~created_at ~updated_at ~account_id =
  { id; uri; text; created_at; updated_at; account_id }

type follow = {
  id : int;
  created_at : Ptime.t;
  updated_at : Ptime.t;
  account_id : int;
  target_account_id : int;
  uri : string;
}

let make_follow ~id ~created_at ~updated_at ~account_id ~target_account_id ~uri
    =
  { id; created_at; updated_at; account_id; target_account_id; uri }

type follow_request = {
  id : int;
  created_at : Ptime.t;
  updated_at : Ptime.t;
  account_id : int;
  target_account_id : int;
  uri : string;
}

let make_follow_request ~id ~created_at ~updated_at ~account_id
    ~target_account_id ~uri =
  { id; created_at; updated_at; account_id; target_account_id; uri }

let now () = Unix.gettimeofday () |> Ptime.of_float_s |> Option.get

module Internal : sig
  val initialize : unit -> unit
  val get_account : id:int -> account Lwt.t
  val get_account_by_username : string -> string -> account option Lwt.t
  val get_account_by_uri : string -> account option Lwt.t
  val upsert_account : account -> account Lwt.t
  val get_user : id:int -> user Lwt.t
  val get_user_by_username : string -> user Lwt.t
  val insert_user : user -> user Lwt.t
  val insert_status : status -> status Lwt.t
  val update_status_uri : status -> status Lwt.t
  val insert_follow : follow -> follow Lwt.t
  val insert_follow_no_conflict : follow -> unit Lwt.t
  val delete_follow_by_uri : string -> unit Lwt.t
  val get_follows_by_target_account_id : int -> follow list Lwt.t

  val get_follow_by_accounts :
    account_id:int -> target_account_id:int -> follow option Lwt.t

  val insert_follow_request : follow_request -> follow_request Lwt.t
  val delete_follow_request : int -> unit Lwt.t
  val get_follow_request_by_uri : string -> follow_request option Lwt.t

  val get_follow_request_by_accounts :
    account_id:int -> target_account_id:int -> follow_request option Lwt.t

  val migrate : unit -> unit Lwt.t
  val rollback : unit -> unit Lwt.t
end = struct
  let global_pool = ref None

  let initialize () =
    match
      Caqti_lwt.connect_pool ~max_size:10 (C.db_url () |> Uri.of_string)
    with
    | Ok pool ->
        global_pool := Some pool;
        ()
    | Error err -> failwith (Caqti_error.show err)

  let global_pool () = !global_pool |> Option.get

  let do_query q =
    match%lwt Caqti_lwt.Pool.use q (global_pool ()) with
    | Ok v -> Lwt.return v
    | Error e ->
        let msg = Caqti_error.show e in
        Log.err (fun m -> m "Query failed: %s" msg);
        failwith msg

  let get_account ~id =
    [%rapper
      get_one
        {|
    SELECT
      @int{id},
      @string{username},
      @string?{domain},
      @string?{private_key},
      @string{public_key},
      @string{display_name},
      @string{uri},
      @string?{url},
      @string{inbox_url},
      @string{followers_url},
      @ptime{created_at},
      @ptime{updated_at}
    FROM
      accounts
    WHERE
      id = %int{id}
    |}
        record_out]
      ~id
    |> do_query

  let get_account_by_username domain username =
    [%rapper
      get_opt
        {|
    SELECT
      @int{id},
      @string{username},
      @string?{domain},
      @string?{private_key},
      @string{public_key},
      @string{display_name},
      @string{uri},
      @string?{url},
      @string{inbox_url},
      @string{followers_url},
      @ptime{created_at},
      @ptime{updated_at}
    FROM
      accounts
    WHERE
      username = %string{username} AND
      COALESCE(domain, '') = %string{domain}
    |}
        record_out]
      ~username ~domain
    |> do_query

  let get_account_by_uri uri =
    [%rapper
      get_opt
        {|
    SELECT
      @int{id},
      @string{username},
      @string?{domain},
      @string?{private_key},
      @string{public_key},
      @string{display_name},
      @string{uri},
      @string?{url},
      @string{inbox_url},
      @string{followers_url},
      @ptime{created_at},
      @ptime{updated_at}
    FROM
      accounts
    WHERE
      uri = %string{uri}
    |}
        record_out]
      ~uri
    |> do_query

  let upsert_account a =
    ([%rapper
       get_one
         {|
        INSERT INTO accounts (
          username,
          domain,
          private_key,
          public_key,
          display_name,
          uri,
          url,
          inbox_url,
          followers_url,
          created_at,
          updated_at)
        VALUES (
          %string{username},
          %string?{domain},
          %string?{private_key},
          %string{public_key},
          %string{display_name},
          %string{uri},
          %string?{url},
          %string{inbox_url},
          %string{followers_url},
          %ptime{created_at},
          %ptime{updated_at})
        ON CONFLICT (username, domain)
        DO UPDATE SET
          username = %string{username},
          domain = %string?{domain},
          private_key = %string?{private_key},
          public_key = %string{public_key},
          display_name = %string{display_name},
          uri = %string{uri},
          url = %string?{url},
          inbox_url = %string{inbox_url},
          followers_url = %string{followers_url},
          created_at = %ptime{created_at},
          updated_at = %ptime{updated_at}
        RETURNING
          @int{id},
          @string{username},
          @string?{domain},
          @string?{private_key},
          @string{public_key},
          @string{display_name},
          @string{uri},
          @string?{url},
          @string{inbox_url},
          @string{followers_url},
          @ptime{created_at},
          @ptime{updated_at}
      |}
         record_in record_out]
       a [@warning "-9"])
    |> do_query

  let get_user ~id =
    [%rapper
      get_one
        {|
        SELECT
          @int{users.id},
          @string{users.email},
          @ptime{users.created_at},
          @ptime{users.updated_at},
          @int{users.account_id}
        FROM users
        WHERE id = %int{id}
      |}
        function_out]
      make_user ~id
    |> do_query

  let get_user_by_username username =
    [%rapper
      get_one
        {|
        SELECT
          @int{users.id},
          @string{users.email},
          @ptime{users.created_at},
          @ptime{users.updated_at},
          @int{users.account_id}
        FROM users
        INNER JOIN accounts ON users.account_id = accounts.id
        WHERE accounts.username = %string{username}
      |}
        function_out]
      make_user ~username
    |> do_query

  let insert_user u =
    ([%rapper
       get_one
         {|
         INSERT INTO users (
           email,
           created_at,
           updated_at,
           account_id)
         VALUES (
           %string{email},
           %ptime{created_at},
           %ptime{updated_at},
           %int{account_id})
         RETURNING
           @int{id},
           @string{email},
           @ptime{created_at},
           @ptime{updated_at},
           @int{account_id}
       |}
         record_in function_out]
       make_user u [@warning "-9"])
    |> do_query

  let insert_status s =
    ([%rapper
       get_one
         {|
INSERT INTO statuses (
  uri,
  text,
  created_at,
  updated_at,
  account_id)
VALUES (
  %string{uri},
  %string{text},
  %ptime{created_at},
  %ptime{updated_at},
  %int{account_id})
RETURNING
  @int{id},
  @string{uri},
  @string{text},
  @ptime{created_at},
  @ptime{updated_at},
  @int{account_id}
    |}
         record_in function_out]
       make_status s [@warning "-9"])
    |> do_query

  let update_status_uri (s : status) =
    [%rapper
      get_one
        {|
UPDATE statuses SET
  uri = %string{uri}
WHERE
  id = %int{id}
RETURNING
  @int{id},
  @string{uri},
  @string{text},
  @ptime{created_at},
  @ptime{updated_at},
  @int{account_id}
        |}
        function_out]
      ~id:s.id ~uri:s.uri make_status
    |> do_query

  let get_follow_by_accounts ~account_id ~target_account_id =
    [%rapper
      get_opt
        {|
SELECT
  @int{id},
  @ptime{created_at},
  @ptime{updated_at},
  @int{account_id},
  @int{target_account_id},
  @string{uri}
FROM follows
WHERE
  account_id = %int{account_id} AND
  target_account_id = %int{target_account_id}
    |}
        function_out]
      make_follow ~account_id ~target_account_id
    |> do_query

  let get_follows_by_target_account_id (aid : int) =
    [%rapper
      get_many
        {|
SELECT
  @int{id},
  @ptime{created_at},
  @ptime{updated_at},
  @int{account_id},
  @int{target_account_id},
  @string{uri}
FROM follows
WHERE follows.target_account_id = %int{target_account_id}
    |}
        function_out]
      make_follow ~target_account_id:aid
    |> do_query

  let insert_follow (f : follow) =
    ([%rapper
       get_one
         {|
INSERT INTO follows (
  created_at,
  updated_at,
  account_id,
  target_account_id,
  uri)
VALUES (
  %ptime{created_at},
  %ptime{updated_at},
  %int{account_id},
  %int{target_account_id},
  %string{uri})
RETURNING
  @int{id},
  @ptime{created_at},
  @ptime{updated_at},
  @int{account_id},
  @int{target_account_id},
  @string{uri}
    |}
         function_out]
       make_follow ~created_at:f.created_at ~updated_at:f.updated_at
       ~account_id:f.account_id ~target_account_id:f.target_account_id
       ~uri:f.uri [@warning "-9"])
    |> do_query

  let insert_follow_no_conflict (f : follow) =
    ([%rapper
       execute
         {|
INSERT INTO follows (
  created_at,
  updated_at,
  account_id,
  target_account_id,
  uri)
VALUES (
  %ptime{created_at},
  %ptime{updated_at},
  %int{account_id},
  %int{target_account_id},
  %string{uri})
ON CONFLICT (account_id, target_account_id) DO NOTHING
    |}]
       ~created_at:f.created_at ~updated_at:f.updated_at
       ~account_id:f.account_id ~target_account_id:f.target_account_id
       ~uri:f.uri [@warning "-9"])
    |> do_query

  let delete_follow_by_uri (uri : string) =
    [%rapper execute {|
DELETE FROM follows
WHERE uri = %string{uri}
    |}]
      ~uri
    |> do_query

  let insert_follow_request (f : follow_request) : follow_request Lwt.t =
    ([%rapper
       get_one
         {|
INSERT INTO follow_requests (
  created_at,
  updated_at,
  account_id,
  target_account_id,
  uri)
VALUES (
  %ptime{created_at},
  %ptime{updated_at},
  %int{account_id},
  %int{target_account_id},
  %string{uri})
RETURNING
  @int{id},
  @ptime{created_at},
  @ptime{updated_at},
  @int{account_id},
  @int{target_account_id},
  @string{uri}
    |}
         function_out]
       make_follow_request ~created_at:f.created_at ~updated_at:f.updated_at
       ~account_id:f.account_id ~target_account_id:f.target_account_id
       ~uri:f.uri [@warning "-9"])
    |> do_query

  let delete_follow_request (id : int) : unit Lwt.t =
    [%rapper execute {|
DELETE FROM follow_requests
WHERE id = %int{id}
    |}]
      ~id
    |> do_query

  let get_follow_request_by_uri (uri : string) =
    [%rapper
      get_opt
        {|
SELECT
  @int{id},
  @ptime{created_at},
  @ptime{updated_at},
  @int{account_id},
  @int{target_account_id},
  @string{uri}
FROM follow_requests
WHERE
  uri = %string{uri}
    |}
        function_out]
      make_follow_request ~uri
    |> do_query

  let get_follow_request_by_accounts ~account_id ~target_account_id =
    [%rapper
      get_opt
        {|
SELECT
  @int{id},
  @ptime{created_at},
  @ptime{updated_at},
  @int{account_id},
  @int{target_account_id},
  @string{uri}
FROM follow_requests
WHERE
  account_id = %int{account_id} AND
  target_account_id = %int{target_account_id}
    |}
        function_out]
      make_follow_request ~account_id ~target_account_id
    |> do_query

  module Migration = struct
    module type S = sig
      val up :
        (unit ->
        (module Rapper_helper.CONNECTION) ->
        (unit, [> Caqti_error.call_or_retrieve ]) result Lwt.t)
        list

      val down :
        (unit ->
        (module Rapper_helper.CONNECTION) ->
        (unit, [> Caqti_error.call_or_retrieve ]) result Lwt.t)
        list
    end

    module M20221230_220000_Big_Bang : S = struct
      let up =
        [
          [%rapper
            execute
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
              );
            |}];
          [%rapper
            execute
              {|
              CREATE TABLE users (
                id SERIAL PRIMARY KEY,
                email TEXT NOT NULL,
                created_at TIMESTAMP WITHOUT TIME ZONE NOT NULL,
                updated_at TIMESTAMP WITHOUT TIME ZONE NOT NULL,
                account_id BIGINT NOT NULL,

                FOREIGN KEY (account_id) REFERENCES accounts (id) ON UPDATE CASCADE ON DELETE CASCADE
              );
            |}];
          [%rapper
            execute
              {|
              CREATE TABLE statuses (
                id SERIAL PRIMARY KEY,
                uri TEXT NOT NULL,
                text TEXT NOT NULL,
                created_at TIMESTAMP WITHOUT TIME ZONE NOT NULL,
                updated_at TIMESTAMP WITHOUT TIME ZONE NOT NULL,
                account_id BIGINT NOT NULL,

                FOREIGN KEY (account_id) REFERENCES accounts (id) ON UPDATE CASCADE ON DELETE CASCADE
              );
            |}];
          [%rapper
            execute
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
                )
            |}];
          [%rapper
            execute
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
                )
            |}];
        ]

      let down =
        [
          [%rapper execute {| DROP TABLE follow_requests; |}];
          [%rapper execute {| DROP TABLE follows; |}];
          [%rapper execute {| DROP TABLE statuses; |}];
          [%rapper execute {| DROP TABLE users; |}];
          [%rapper execute {| DROP TABLE accounts; |}];
        ]
    end

    let all : (int * (module S)) list =
      [ (20221230220000, (module M20221230_220000_Big_Bang)) ]

    let process kind =
      let src =
        let c x = `C x in
        all
        |> List.map (fun (id, (module M : S)) ->
               match kind with
               | `Migrate -> `MId id :: (M.up |> List.map c)
               | `Rollback -> `RId id :: (M.down |> List.map c))
        |> List.flatten
      in
      let rec aux = function
        | [] -> Lwt.return_unit
        | `MId id :: vs ->
            Log.info (fun m -> m "Migrate %d" id);
            aux vs
        | `RId id :: vs ->
            Log.info (fun m -> m "Rollback %d" id);
            aux vs
        | `C v :: vs ->
            let%lwt () = v () |> do_query in
            aux vs
      in
      aux src
  end

  let migrate () = Migration.process `Migrate
  let rollback () = Migration.process `Rollback
end

include Internal
