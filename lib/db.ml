include Sqlx.Engine.Make (Sqlx.Driver_pg)

let maybe_no_row e =
  match%lwt e with
  | exception Sqlx.Error.NoRowFound -> Lwt.return_none
  | res -> Lwt.return_some res

let debug_drop_all_tables_in_db () =
  e @@ fun c ->
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
END $$|}

include Model
open Util

let register_user ~username ~display_name ~email ~password =
  let now = Ptime.now () in
  let created_at, updated_at = (now, now) in
  let private_key, public_key = Yume.Signature.generate_keypair () in
  let public_key = Yume.Signature.encode_public_key public_key in
  let private_key = Yume.Signature.encode_private_key private_key in
  let uri = Config.url [ "users"; username ] in
  let inbox_url = uri ^/ "inbox" in
  let outbox_url = uri ^/ "outbox" in
  let followers_url = uri ^/ "followers" in
  let shared_inbox_url = Config.url [ "inbox" ] in
  let encrypted_password = Bcrypt.(hash password |> string_of_hash) in
  let%lwt a =
    e
      Account.(
        make ~username ~public_key ~private_key ~display_name ~uri ~inbox_url
          ~outbox_url ~followers_url ~created_at ~updated_at ~shared_inbox_url
          ~header_remote_url:(Config.default_header_url ())
          ~avatar_remote_url:(Config.default_avatar_url ())
          ~note:"" ()
        |> save_one)
  in
  let%lwt u =
    let created_at, updated_at = (now, now) in
    e
      User.(
        make ~email ~created_at ~updated_at ~account_id:a#id ~encrypted_password
          ()
        |> save_one)
  in
  Lwt.return (a, u)

let initialize () = initialize (Config.db_url ())
let transaction f = e (fun c -> c#transaction f)
let e x = Lwt_eio.run_lwt @@ fun () -> e x

(* utility functions *)

let is_following ~(account_id : Account.ID.t)
    ~(target_account_id : Account.ID.t) : bool =
  e @@ Follow.get_many ~account_id ~target_account_id <> []

let has_mention ~(status_id : Status.ID.t) ~(account_id : Account.ID.t) : bool =
  e
  @@ Mention.get_many ~status_id:(Some status_id) ~account_id:(Some account_id)
  <> []
