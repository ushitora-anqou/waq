let up (c : Sqlx.Connection.t) =
  c#execute
    {|
CREATE TABLE mentions (
  id SERIAL PRIMARY KEY,
  created_at TIMESTAMP WITHOUT TIME ZONE NOT NULL,
  updated_at TIMESTAMP WITHOUT TIME ZONE NOT NULL,
  status_id BIGINT,
  account_id BIGINT,

  FOREIGN KEY ( account_id ) REFERENCES accounts ( id ) ON DELETE CASCADE,
  FOREIGN KEY ( status_id ) REFERENCES statuses ( id ) ON DELETE CASCADE
)|}

let down (c : Sqlx.Connection.t) = c#execute {|DROP TABLE mentions|}
