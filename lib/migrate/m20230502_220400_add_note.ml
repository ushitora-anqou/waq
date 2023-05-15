open Sqlx.Migration.Helper

let change =
  add_column ~table_name:"accounts" ~name:"note"
    ~spec:"TEXT NOT NULL DEFAULT ''"
