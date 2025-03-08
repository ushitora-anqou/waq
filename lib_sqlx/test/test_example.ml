open Sqlx
open Lwt.Infix

[@@@warning "-32"]

(* Define two RDB schemas (accounts and statuses) *)
[%%sqlx.schemas
module rec Account = struct
  name "accounts"

  class type t = object
    (* Table `accounts` has 3 columns ... *)
    val username : string
    val domain : string option
    val display_name : string

    (* ... and also has many `statuses` thorugh foreign key *)
    val statuses : Status.t list [@@foreign_key `account_id]
  end
end

and Status = struct
  name "statuses"

  class type t = object
    (* Table `statuses` has 4 columns *)
    val text : string
    val in_reply_to_id : ID.t option
    val reblog_of_id : ID.t option
    val account_id : Account.ID.t
  end
end]

module Db = struct
  include Engine.Make (Driver_pg)
end

(**)

(* Insert some records into table `statuses` for testing *)
let insert_some_statuses () : unit Lwt.t =
  (* Insert two accounts (`a1` and `a2`) *)
  let%lwt a1 =
    Db.e Account.(make ~username:"user1" ~display_name:"User 1" () |> save_one)
  in
  let%lwt a2 =
    Db.e
      Account.(
        make ~username:"user2" ~domain:"example.com" ~display_name:"User 2" ()
        |> save_one)
  in
  (* Insert two statuses published by the accounts `a1` and `a2`. The second status is a reply to the first one.  *)
  let%lwt s1 =
    Db.e Status.(make ~account_id:a1#id ~text:"Hello" () |> save_one)
  in
  let%lwt _ =
    Db.e
      Status.(
        make ~account_id:a2#id ~text:"World" ~in_reply_to_id:s1#id ()
        |> save_one)
  in
  Lwt.return_unit
[@@warning "-8"]

(* Select all statuses that `username` has replied *)
let statuses_replied_by ~(username : string) : Status.t list Lwt.t =
  let%lwt acct =
    Db.e Account.(get_one ~username ~preload:[ `statuses [ `in_reply_to [] ] ])
  in
  acct#statuses |> List.filter_map (fun s -> s#in_reply_to) |> Lwt.return

(**)

let setup () =
  Db.e @@ fun c ->
  c#execute
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
END $$|};%lwt
  c#execute
    {|
CREATE TABLE accounts (
  id SERIAL PRIMARY KEY,
  username TEXT NOT NULL,
  domain TEXT,
  display_name TEXT NOT NULL,
  created_at TIMESTAMP WITHOUT TIME ZONE NOT NULL,
  updated_at TIMESTAMP WITHOUT TIME ZONE NOT NULL
)|};%lwt
  c#execute
    {|
CREATE TABLE statuses (
  id SERIAL PRIMARY KEY,
  created_at TIMESTAMP WITHOUT TIME ZONE NOT NULL,
  updated_at TIMESTAMP WITHOUT TIME ZONE NOT NULL,
  text TEXT NOT NULL,
  account_id BIGINT NOT NULL,
  in_reply_to_id BIGINT,
  reblog_of_id BIGINT,

  FOREIGN KEY (account_id) REFERENCES accounts (id) ON UPDATE CASCADE ON DELETE CASCADE,
  FOREIGN KEY (in_reply_to_id) REFERENCES statuses (id) ON DELETE SET NULL,
  FOREIGN KEY (reblog_of_id) REFERENCES statuses (id) ON DELETE CASCADE
)|}

let test_statuses_replied_by _ _ =
  setup ();%lwt
  insert_some_statuses ();%lwt
  ( statuses_replied_by ~username:"user2" >|= function
    | [ s ] when s#text = "Hello" -> ()
    | _ -> assert false );%lwt
  Lwt.return_unit

open Common

let () =
  Logs.set_reporter (Logs_fmt.reporter ());
  with_postgres ~host_port:54320
    ~container_name:"waq-test-sqlx-postgres-example"
  @@ fun url ->
  Db.initialize url;
  Lwt_main.run
  @@ Alcotest_lwt.run ~and_exit:false "sqlx"
       [
         ( "replied_statuses_by_account",
           [ Alcotest_lwt.test_case "case1" `Quick test_statuses_replied_by ] );
       ]
