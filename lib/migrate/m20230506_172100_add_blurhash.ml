open Sqlx.Migration.Helper

let change =
  add_column ~table_name:"media_attachments" ~name:"blurhash" ~spec:"TEXT"
