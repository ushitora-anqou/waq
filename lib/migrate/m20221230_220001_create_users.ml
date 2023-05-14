let up (c : Sqlx.Connection.t) =
  c#execute
    {|
CREATE TABLE users (
  id SERIAL PRIMARY KEY,
  email TEXT NOT NULL,
  created_at TIMESTAMP WITHOUT TIME ZONE NOT NULL,
  updated_at TIMESTAMP WITHOUT TIME ZONE NOT NULL,
  encrypted_password TEXT NOT NULL,
  account_id BIGINT NOT NULL,

  UNIQUE (email),
  FOREIGN KEY (account_id) REFERENCES accounts (id) ON UPDATE CASCADE ON DELETE CASCADE
)|}

let down (c : Sqlx.Connection.t) = c#execute {|DROP TABLE users|}

open Sqlx.Migration.Helper

let change =
  create_table ~table_name:"users"
    ~schema:
      [
        "email TEXT NOT NULL";
        "encrypted_password TEXT NOT NULL";
        "account_id BIGINT NOT NULL";
        "UNIQUE (email)";
        (* *)
        "FOREIGN KEY (account_id) REFERENCES accounts (id) ON UPDATE CASCADE \
         ON DELETE CASCADE";
      ]
