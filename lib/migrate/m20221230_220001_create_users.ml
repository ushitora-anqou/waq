let up c =
  Sql.execute c
    {|
CREATE TABLE users (
  id SERIAL PRIMARY KEY,
  email TEXT NOT NULL,
  created_at TIMESTAMP WITHOUT TIME ZONE NOT NULL,
  updated_at TIMESTAMP WITHOUT TIME ZONE NOT NULL,
  encrypted_password TEXT NOT NULL,
  account_id BIGINT NOT NULL,

  FOREIGN KEY (account_id) REFERENCES accounts (id) ON UPDATE CASCADE ON DELETE CASCADE
)|}

let down c = Sql.execute c {|DROP TABLE users|}
