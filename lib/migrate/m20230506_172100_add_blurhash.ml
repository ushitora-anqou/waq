let up (c : Sqlx.Connection.t) =
  c#execute {|ALTER TABLE media_attachments ADD COLUMN blurhash TEXT|}

let down (c : Sqlx.Connection.t) =
  c#execute {|ALTER TABLE media_attachments DROP COLUMN blurhash|}
