open Migrations

let config = Sqlx.Migration.{ schema_migrations = "waq_schema_migrations" }

let verify_migration_status () =
  Db.e @@ Sqlx.Migration.verify_migration_status ~config ~migrations

let migrate () = Db.e @@ Sqlx.Migration.migrate ~config ~migrations

let rollback ?(n = 1) () =
  Db.e @@ Sqlx.Migration.rollback ~config ~migrations ~n
