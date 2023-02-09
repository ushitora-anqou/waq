let up c =
  Sql.execute c
    {|
CREATE TABLE favourites (
  id SERIAL PRIMARY KEY,
  created_at TIMESTAMP WITHOUT TIME ZONE NOT NULL,
  updated_at TIMESTAMP WITHOUT TIME ZONE NOT NULL,
  account_id BIGINT NOT NULL,
  status_id BIGINT NOT NULL,

  FOREIGN KEY (account_id) REFERENCES accounts(id) ON DELETE CASCADE,
  FOREIGN KEY (status_id) REFERENCES statuses(id) ON DELETE CASCADE,
  UNIQUE (account_id, status_id)
)|}

let down c = Sql.execute c {|DROP TABLE favourites|}
