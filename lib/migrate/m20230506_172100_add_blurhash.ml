let up (c : Sqlx.Connection.t) =
  c#execute {|ALTER TABLE media_attachments ADD COLUMN blurhash TEXT|}

let down (c : Sqlx.Connection.t) =
  c#execute {|ALTER TABLE media_attachments DROP COLUMN blurhash|}

open Sqlx.Migration.Helper

let change =
  add_column ~table_name:"media_attachments" ~name:"blurhash" ~spec:"TEXT"
