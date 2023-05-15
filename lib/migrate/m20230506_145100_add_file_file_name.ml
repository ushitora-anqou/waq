open Sqlx.Migration.Helper

let change =
  add_column ~table_name:"media_attachments" ~name:"file_file_name" ~spec:"TEXT"
