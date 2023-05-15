open Sqlx.Migration.Helper

let change =
  add_column ~table_name:"preview_cards" ~name:"blurhash" ~spec:"TEXT"
