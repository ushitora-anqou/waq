let up (c : Sqlx.Connection.t) =
  c#execute
    {|
CREATE TABLE oauth_applications (
  id SERIAL PRIMARY KEY,
  name TEXT NOT NULL,
  uid TEXT NOT NULL,
  secret TEXT NOT NULL,
  redirect_uri TEXT NOT NULL,
  scopes TEXT NOT NULL,
  created_at TIMESTAMP WITHOUT TIME ZONE,
  updated_at TIMESTAMP WITHOUT TIME ZONE,

  UNIQUE (uid)
)|}

let down (c : Sqlx.Connection.t) = c#execute {|DROP TABLE oauth_applications|}

open Sqlx.Migration.Helper

let change =
  create_table ~table_name:"oauth_applications"
    ~schema:
      [
        {|name TEXT NOT NULL|};
        {|uid TEXT NOT NULL|};
        {|secret TEXT NOT NULL|};
        {|redirect_uri TEXT NOT NULL|};
        {|scopes TEXT NOT NULL|};
        (* *)
        {|UNIQUE (uid)|};
      ]
