let up (c : Sqlx.Connection.t) =
  c#execute
    {|ALTER TABLE statuses ADD COLUMN spoiler_text TEXT NOT NULL DEFAULT ''|}

let down (c : Sqlx.Connection.t) =
  c#execute {|ALTER TABLE statuses DROP COLUMN spoiler_text|}

open Sqlx.Migration.Helper

let change =
  add_column ~table_name:"statuses" ~name:"spoiler_text"
    ~spec:"TEXT NOT NULL DEFAULT ''"
