open Sqlx
open Lwt.Infix

[@@@warning "-32"]

[%%sqlx.schemas
module rec Account = struct
  name "accounts"

  class type t =
    object
      val username : string
      val domain : string option
      val display_name : string
      val statuses : Status.t list [@@foreign_key `account_id]
    end
end

and Status = struct
  name "statuses"

  class type t =
    object
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

let insert_some_statuses () : unit Lwt.t =
  let%lwt a =
    Db.e
      Account.(
        make ~username:"anqou" ~domain:"example.com" ~display_name:"Anqou" ()
        |> save_one)
  in
  let%lwt s1 =
    Db.e Status.(make ~account_id:a#id ~text:"Hello" () |> save_one)
  in
  let%lwt _ =
    Db.e
      Status.(
        make ~account_id:a#id ~text:"World" ~in_reply_to_id:s1#id () |> save_one)
  in
  Lwt.return_unit
  [@@warning "-8"]

let replied_statuses_by_account ~(username : string) : Status.t list Lwt.t =
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

let test_replied_statuses_by_account _ _ =
  setup ();%lwt
  insert_some_statuses ();%lwt
  (replied_statuses_by_account ~username:"anqou" >|= function
   | [ s ] when s#text = "Hello" -> ()
   | _ -> assert false);%lwt
  Lwt.return_unit

let () =
  Logq.(add_reporter (make_reporter ~l:Debug ()));
  Db.initialize (Sys.getenv "SQLX_TEST_DB_URL1");
  Lwt_main.run
  @@ Alcotest_lwt.run "sqlx"
       [
         ( "replied_statuses_by_account",
           [
             Alcotest_lwt.test_case "case1" `Quick
               test_replied_statuses_by_account;
           ] );
       ]
