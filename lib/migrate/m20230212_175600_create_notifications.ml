let up c =
  Sql.execute c
    {|
CREATE TABLE notifications (
  id SERIAL PRIMARY KEY,
  activity_id BIGINT NOT NULL,
  activity_type TEXT NOT NULL,
  created_at TIMESTAMP WITHOUT TIME ZONE NOT NULL,
  updated_at TIMESTAMP WITHOUT TIME ZONE NOT NULL,
  account_id BIGINT NOT NULL,
  from_account_id BIGINT NOT NULL,
  type TEXT NOT NULL,

  FOREIGN KEY (account_id) REFERENCES accounts (id) ON DELETE CASCADE,
  FOREIGN KEY (from_account_id) REFERENCES accounts (id) ON DELETE CASCADE
)|}

let down c = Sql.execute c {|DROP TABLE notifications|}
