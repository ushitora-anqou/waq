let up (c : Sqlx.Connection.t) =
  c#execute {|ALTER TABLE media_attachments ADD COLUMN file_file_name TEXT|}

let down (c : Sqlx.Connection.t) =
  c#execute {|ALTER TABLE media_attachments DROP COLUMN file_file_name|}

open Sqlx.Migration.Helper

let change =
  add_column ~table_name:"media_attachments" ~name:"file_file_name" ~spec:"TEXT"
