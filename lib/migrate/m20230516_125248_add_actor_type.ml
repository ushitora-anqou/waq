open Sqlx.Migration.Helper

let change = add_column ~table_name:"accounts" ~name:"actor_type" ~spec:"TEXT"
