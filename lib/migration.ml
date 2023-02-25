module type S = sig
  val up : Sql.connection -> unit Lwt.t
  val down : Sql.connection -> unit Lwt.t
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
    ]

let migrate () =
  Db.do_query @@ fun c ->
  all
  |> Lwt_list.iter_s (fun (id, (module M : S)) ->
         Logq.info (fun m -> m "Migrate %d" id);
         M.up c)

let rollback () =
  Db.do_query @@ fun c ->
  all |> List.rev
  |> Lwt_list.iter_s (fun (id, (module M : S)) ->
         Logq.info (fun m -> m "Rollback %d" id);
         M.down c)
