module type S = sig
  val up : Sql.connection -> unit Lwt.t
  val down : Sql.connection -> unit Lwt.t
end

let all : (int * (module S)) list =
  [
    (20221230220000, (module Migrate.M20221230_220000_create_accounts));
    (20221230220001, (module Migrate.M20221230_220001_create_users));
    (20221230220002, (module Migrate.M20221230_220002_create_statuses));
    (20221230220003, (module Migrate.M20221230_220003_create_follows));
    (20221230220004, (module Migrate.M20221230_220004_create_follow_requests));
    (20230121195200, (module Migrate.M20230121_195200_create_oauth_applications));
    ( 20230121195201,
      (module Migrate.M20230121_195201_create_oauth_access_tokens) );
    ( 20230122_183000,
      (module Migrate.M20230122_183000_create_oauth_access_grants) );
  ]

let migrate () =
  Db.do_query @@ fun c ->
  all
  |> Lwt_list.iter_s (fun (id, (module M : S)) ->
         Log.info (fun m -> m "Migrate %d" id);
         M.up c)

let rollback () =
  Db.do_query @@ fun c ->
  all |> List.rev
  |> Lwt_list.iter_s (fun (id, (module M : S)) ->
         Log.info (fun m -> m "Rollback %d" id);
         M.down c)
