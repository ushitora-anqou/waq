open Sqlx.Migration.Helper

let change =
  add_column ~table_name:"statuses" ~name:"visibility" ~spec:"INTEGER NOT NULL"
