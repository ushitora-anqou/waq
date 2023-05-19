open Sqlx.Migration.Helper

let change = add_column ~table_name:"statuses" ~name:"url" ~spec:"TEXT"