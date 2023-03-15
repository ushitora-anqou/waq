open Lwt.Infix
open Util

module type S = sig
  val up : Sqlx.Connection.t -> unit Lwt.t
  val down : Sqlx.Connection.t -> unit Lwt.t
end

let all : (int * (module S)) list =
  Migrate.
    [
      (20221230_220000, (module M20221230_220000_create_accounts));
      (20221230_220001, (module M20221230_220001_create_users));
      (20221230_220002, (module M20221230_220002_create_statuses));
      (20221230_220003, (module M20221230_220003_create_follows));
      (20221230_220004, (module M20221230_220004_create_follow_requests));
      (20230121_195200, (module M20230121_195200_create_oauth_applications));
      (20230121_195201, (module M20230121_195201_create_oauth_access_tokens));
      (20230122_183000, (module M20230122_183000_create_oauth_access_grants));
      (20230209_204400, (module M20230209_204400_create_favourites));
      (20230212_175600, (module M20230212_175600_create_notifications));
      (20230225_173800, (module M20230225_173800_create_account_stats));
      (20230225_174100, (module M20230225_174100_create_status_stats));
      (20230315_100000, (module M20230315_100000_add_updated_at_column));
    ]

let get_commited_versions (c : Sqlx.Connection.t) =
  c#execute
    "CREATE TABLE IF NOT EXISTS waq_schema_migrations ( version BIGINT PRIMARY \
     KEY )";%lwt
  c#query "SELECT version FROM waq_schema_migrations ORDER BY version"
  >|= List.map (function
        | [ ("version", v) ] -> Sqlx.Value.expect_int v
        | _ -> assert false)

let verify_migration_status c =
  get_commited_versions c >|= fun versions ->
  let versions' = all |> List.map fst in
  if versions <> versions' then
    failwith "Verification of migration status failed"
(*
  (* versions' should be a prefix of versions *)
  let rec aux = function
    | [], _ -> true
    | x :: xs, y :: ys when x = y -> aux (xs, ys)
    | _ -> false
  in
  aux (versions', versions)
*)

let migrate () =
  (* FIXME: Not sure it's safe when it's called twice in parallel *)
  Db.e @@ fun c ->
  let%lwt versions = get_commited_versions c in
  let rec aux = function
    | x :: commited, (y, _) :: cands when x = y -> aux (commited, cands)
    | [], cands ->
        cands
        |> Lwt_list.iter_s (fun (id, (module M : S)) ->
               Logq.info (fun m -> m "Migrate %d" id);
               c#execute "BEGIN";%lwt
               try
                 M.up c;%lwt
                 c#execute "INSERT INTO waq_schema_migrations VALUES ($1)"
                   ~p:[ `Int id ];%lwt
                 c#execute "COMMIT"
               with _ -> c#execute "ROLLBACK")
    | _ -> failwith "Migration error: current status is invalid"
  in
  aux (versions, all)

let rollback ?(n = 1) () =
  Db.e @@ fun c ->
  let%lwt versions = get_commited_versions c in
  versions |> List.rev |> List.take n
  |> Lwt_list.iter_s @@ fun last_v ->
     let (module M : S) = all |> List.assoc last_v in
     c#execute "BEGIN";%lwt
     try
       M.down c;%lwt
       c#execute "DELETE FROM waq_schema_migrations WHERE version = $1"
         ~p:[ `Int last_v ];%lwt
       c#execute "COMMIT"
     with _ -> c#execute "ROLLBACK"
