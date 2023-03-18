let up (c : Sqlx.Connection.t) =
  c#execute {|ALTER TABLE accounts ADD COLUMN avatar_remote_url TEXT|};%lwt
  c#execute
    {|ALTER TABLE accounts ADD COLUMN header_remote_url TEXT NOT NULL DEFAULT ''|}

let down (c : Sqlx.Connection.t) =
  c#execute {|ALTER TABLE accounts DROP COLUMN avatar_remote_url|};%lwt
  c#execute {|ALTER TABLE accounts DROP COLUMN header_remote_url|}
