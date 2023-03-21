let config = Sqlx.Migration.{ schema_migrations = "waq_schema_migrations" }

let migrations : (int * (module Sqlx.Migration.S)) list =
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
      (20230318_232000, (module M20230318_232000_add_avatar_header_remote_url));
      ( 20230321_215500,
        (module M20230321_215500_create_unique_index_on_accounts_username_domain)
      );
      (20230321_215501, (module M20230321_215501_create_media_attachments));
    ]

let verify_migration_status () =
  Db.e @@ Sqlx.Migration.verify_migration_status ~config ~migrations

let migrate () = Db.e @@ Sqlx.Migration.migrate ~config ~migrations

let rollback ?(n = 1) () =
  Db.e @@ Sqlx.Migration.rollback ~config ~migrations ~n
