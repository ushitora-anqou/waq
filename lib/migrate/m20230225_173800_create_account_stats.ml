let up c =
  Sql.execute c
    {|
CREATE TABLE account_stats (
  id SERIAL PRIMARY KEY,
  account_id BIGINT NOT NULL,
  statuses_count BIGINT NOT NULL,
  following_count BIGINT NOT NULL,
  followers_count BIGINT NOT NULL,
  created_at TIMESTAMP WITHOUT TIME ZONE NOT NULL,
  updated_at TIMESTAMP WITHOUT TIME ZONE NOT NULL,
  last_status_at TIMESTAMP WITHOUT TIME ZONE,

  FOREIGN KEY (account_id) REFERENCES accounts(id) ON DELETE CASCADE,
  UNIQUE (account_id)
)|}

let down c = Sql.execute c {|DROP TABLE account_stats|}
