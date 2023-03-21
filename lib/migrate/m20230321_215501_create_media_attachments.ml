let up (c : Sqlx.Connection.t) =
  c#execute
    {|
CREATE TABLE media_attachments (
  id SERIAL PRIMARY KEY,
  created_at TIMESTAMP WITHOUT TIME ZONE NOT NULL,
  updated_at TIMESTAMP WITHOUT TIME ZONE NOT NULL,
  status_id BIGINT,
  account_id BIGINT,
  remote_url TEXT NOT NULL,
  type INTEGER NOT NULL,

  FOREIGN KEY ( account_id ) REFERENCES accounts ( id ) ON DELETE SET NULL,
  FOREIGN KEY ( status_id ) REFERENCES statuses ( id ) ON DELETE SET NULL
)|}

let down (c : Sqlx.Connection.t) = c#execute {|DROP TABLE media_attachments|}
