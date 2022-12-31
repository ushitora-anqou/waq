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
  private_key : string option;
  public_key : string;
  display_name : string;
  created_at : Ptime.t;
  updated_at : Ptime.t;
}
[@@deriving make]

module Internal : sig
  val initialize : unit -> unit
  val get_account : id:int -> account Lwt.t
  val insert_account : account -> account Lwt.t
  val get_user_by_username : string -> user Lwt.t
  val insert_user : user -> user Lwt.t
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
      @string?{private_key},
      @string{public_key},
      @string{display_name},
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

  let insert_account a =
    ([%rapper
       get_one
         {|
        INSERT INTO accounts (
          username,
          private_key,
          public_key,
          display_name,
          created_at,
          updated_at)
        VALUES (
          %string{username},
          %string?{private_key},
          %string{public_key},
          %string{display_name},
          %ptime{created_at},
          %ptime{updated_at})
        RETURNING
          @int{id},
          @string{username},
          @string?{private_key},
          @string{public_key},
          @string{display_name},
          @ptime{created_at},
          @ptime{updated_at}
      |}
         record_in record_out]
       a [@warning "-9"])
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
                username CHARACTER VARYING NOT NULL,
                private_key TEXT,
                public_key TEXT NOT NULL,
                display_name CHARACTER VARYING NOT NULL,
                created_at TIMESTAMP WITHOUT TIME ZONE NOT NULL,
                updated_at TIMESTAMP WITHOUT TIME ZONE NOT NULL
              );
            |}];
          [%rapper
            execute
              {|
              CREATE TABLE users (
                id SERIAL PRIMARY KEY,
                email CHARACTER VARYING NOT NULL,
                created_at TIMESTAMP WITHOUT TIME ZONE NOT NULL,
                updated_at TIMESTAMP WITHOUT TIME ZONE NOT NULL,
                account_id BIGINT NOT NULL,

                FOREIGN KEY (account_id) REFERENCES accounts (id) ON UPDATE CASCADE ON DELETE CASCADE
              );
            |}];
        ]

      let down =
        [
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
