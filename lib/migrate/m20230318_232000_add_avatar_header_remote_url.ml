open Sqlx.Migration.Helper

let change =
  add_column ~table_name:"accounts" ~name:"avatar_remote_url" ~spec:"TEXT"
  *> add_column ~table_name:"accounts" ~name:"header_remote_url"
       ~spec:"TEXT NOT NULL DEFAULT ''"
