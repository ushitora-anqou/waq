let up (c : Sqlx.Connection.t) =
  c#execute {|ALTER TABLE preview_cards ADD COLUMN blurhash TEXT|}

let down (c : Sqlx.Connection.t) =
  c#execute {|ALTER TABLE preview_cards DROP COLUMN blurhash|}
