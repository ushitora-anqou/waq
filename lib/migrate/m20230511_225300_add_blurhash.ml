let up (c : Sqlx.Connection.t) =
  c#execute {|ALTER TABLE preview_cards ADD COLUMN blurhash TEXT|}

let down (c : Sqlx.Connection.t) =
  c#execute {|ALTER TABLE preview_cards DROP COLUMN blurhash|}

open Sqlx.Migration.Helper

let change =
  add_column ~table_name:"preview_cards" ~name:"blurhash" ~spec:"TEXT"
