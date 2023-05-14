let up (c : Sqlx.Connection.t) =
  c#execute
    {|
CREATE UNIQUE INDEX unique_index_accounts_username_domain ON accounts (
  LOWER(username),
  COALESCE(LOWER(domain), '')
)|}

let down (c : Sqlx.Connection.t) =
  c#execute {|DROP INDEX unique_index_accounts_username_domain|}

open Sqlx.Migration.Helper

let change =
  create_unique_index ~name:"unique_index_accounts_username_domain"
    ~table_name:"accounts"
    ~schema:[ {|LOWER(username)|}; {|COALESCE(LOWER(domain), '')|} ]
