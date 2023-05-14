let up (c : Sqlx.Connection.t) =
  c#execute {|ALTER TABLE accounts ADD COLUMN avatar_remote_url TEXT|};%lwt
  c#execute
    {|ALTER TABLE accounts ADD COLUMN header_remote_url TEXT NOT NULL DEFAULT ''|}

let down (c : Sqlx.Connection.t) =
  c#execute {|ALTER TABLE accounts DROP COLUMN avatar_remote_url|};%lwt
  c#execute {|ALTER TABLE accounts DROP COLUMN header_remote_url|}

open Sqlx.Migration.Helper

let change =
  add_column ~table_name:"accounts" ~name:"avatar_remote_url" ~spec:"TEXT"
  *> add_column ~table_name:"accounts" ~name:"header_remote_url"
       ~spec:"TEXT NOT NULL DEFAULT ''"
