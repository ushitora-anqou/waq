module C = Config

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
                id BIGINT PRIMARY KEY,
                username CHARACTER VARYING NOT NULL,
                private_key TEXT,
                public_key TEXT,
                display_name CHARACTER VARYING,
                created_at TIMESTAMP WITHOUT TIME ZONE NOT NULL,
                updated_at TIMESTAMP WITHOUT TIME ZONE NOT NULL
              );
            |}];
        [%rapper
          execute
            {|
              CREATE TABLE users (
                id BIGINT PRIMARY KEY,
                email CHARACTER VARYING NOT NULL,
                created_at TIMESTAMP WITHOUT TIME ZONE NOT NULL,
                updated_at TIMESTAMP WITHOUT TIME ZONE NOT NULL
              );
            |}];
      ]

    let down =
      [
        [%rapper execute {| DROP TABLE accounts; |}];
        [%rapper execute {| DROP TABLE users; |}];
      ]
  end

  let all : (int * (module S)) list =
    [ (20221230220000, (module M20221230_220000_Big_Bang)) ]

  let process kind pool =
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
      | [] -> Lwt.return @@ Ok ()
      | `MId id :: vs ->
          Log.info (fun m -> m "Migrate %d" id);
          aux vs
      | `RId id :: vs ->
          Log.info (fun m -> m "Rollback %d" id);
          aux vs
      | `C v :: vs -> (
          let%lwt v = Caqti_lwt.Pool.use (v ()) pool in
          match v with
          | Ok () -> aux vs
          | Error e ->
              let msg = Caqti_error.show e in
              Log.err (fun m -> m "%s" msg);
              Lwt.return @@ Error msg)
    in
    aux src
end

let global_pool = ref None

let initialize () =
  match Caqti_lwt.connect_pool ~max_size:10 (C.db_url () |> Uri.of_string) with
  | Ok pool ->
      global_pool := Some pool;
      ()
  | Error err -> failwith (Caqti_error.show err)

let global_pool () = !global_pool |> Option.get
let migrate () = Migration.process `Migrate (global_pool ())
let rollback () = Migration.process `Rollback (global_pool ())
