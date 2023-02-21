let up c =
  Sql.execute c
    {|
CREATE TABLE statuses (
  id SERIAL PRIMARY KEY,
  uri TEXT NOT NULL,
  text TEXT NOT NULL,
  created_at TIMESTAMP WITHOUT TIME ZONE NOT NULL,
  updated_at TIMESTAMP WITHOUT TIME ZONE NOT NULL,
  deleted_at TIMESTAMP WITHOUT TIME ZONE,
  in_reply_to_id BIGINT,
  reblog_of_id BIGINT,
  account_id BIGINT NOT NULL,

  FOREIGN KEY (account_id) REFERENCES accounts (id) ON UPDATE CASCADE ON DELETE CASCADE,
  FOREIGN KEY (in_reply_to_id) REFERENCES statuses (id) ON DELETE SET NULL,
  FOREIGN KEY (reblog_of_id) REFERENCES statuses (id) ON DELETE CASCADE,
  UNIQUE (uri)
)|}

let down c = Sql.execute c {|DROP TABLE statuses|}
