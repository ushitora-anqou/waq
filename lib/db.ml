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

let now () = Unix.gettimeofday () |> Ptime.of_float_s |> Option.get

module Internal : sig
  val initialize : unit -> unit
  val get_account : id:int -> account Lwt.t
  val get_account_by_username : string -> string -> account option Lwt.t
  val upsert_account : account -> account Lwt.t
  val get_user : id:int -> user Lwt.t
  val get_user_by_username : string -> user Lwt.t
  val insert_user : user -> user Lwt.t
  val insert_status : status -> status Lwt.t
  val update_status_uri : status -> status Lwt.t
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

  let update_status_uri s =
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
        ]

      let down =
        [
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
