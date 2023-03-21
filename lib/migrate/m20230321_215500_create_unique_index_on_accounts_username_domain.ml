let up (c : Sqlx.Connection.t) =
  c#execute
    {|
CREATE UNIQUE INDEX unique_index_accounts_username_domain ON accounts (
  LOWER(username),
  COALESCE(LOWER(domain), '')
)|}

let down (c : Sqlx.Connection.t) =
  c#execute {|DROP INDEX unique_index_accounts_username_domain|}
