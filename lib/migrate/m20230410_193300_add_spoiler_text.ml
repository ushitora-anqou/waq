open Sqlx.Migration.Helper

let change =
  add_column ~table_name:"statuses" ~name:"spoiler_text"
    ~spec:"TEXT NOT NULL DEFAULT ''"
