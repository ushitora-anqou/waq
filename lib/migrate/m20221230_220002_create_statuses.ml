let up c =
  Sql.execute c
    {|
CREATE TABLE statuses (
  id SERIAL PRIMARY KEY,
  uri TEXT NOT NULL,
  text TEXT NOT NULL,
  created_at TIMESTAMP WITHOUT TIME ZONE NOT NULL,
  updated_at TIMESTAMP WITHOUT TIME ZONE NOT NULL,
  account_id BIGINT NOT NULL,

  FOREIGN KEY (account_id) REFERENCES accounts (id) ON UPDATE CASCADE ON DELETE CASCADE
)|}

let down c = Sql.execute c {|DROP TABLE statuses|}
