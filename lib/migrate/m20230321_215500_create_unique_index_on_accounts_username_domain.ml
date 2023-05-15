open Sqlx.Migration.Helper

let change =
  create_unique_index ~name:"unique_index_accounts_username_domain"
    ~table_name:"accounts"
    ~schema:[ {|LOWER(username)|}; {|COALESCE(LOWER(domain), '')|} ]
