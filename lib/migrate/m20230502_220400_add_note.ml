let up (c : Sqlx.Connection.t) =
  c#execute {|ALTER TABLE accounts ADD COLUMN note TEXT NOT NULL DEFAULT ''|}

let down (c : Sqlx.Connection.t) =
  c#execute {|ALTER TABLE accounts DROP COLUMN note|}
