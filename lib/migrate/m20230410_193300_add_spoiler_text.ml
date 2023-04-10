let up (c : Sqlx.Connection.t) =
  c#execute
    {|ALTER TABLE statuses ADD COLUMN spoiler_text TEXT NOT NULL DEFAULT ''|}

let down (c : Sqlx.Connection.t) =
  c#execute {|ALTER TABLE statuses DROP COLUMN spoiler_text|}
