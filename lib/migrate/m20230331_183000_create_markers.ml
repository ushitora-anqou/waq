let up (c : Sqlx.Connection.t) =
  c#execute
    {|
CREATE TABLE markers (
  id SERIAL PRIMARY KEY,
  created_at TIMESTAMP WITHOUT TIME ZONE NOT NULL,
  updated_at TIMESTAMP WITHOUT TIME ZONE NOT NULL,
  user_id BIGINT,
  timeline TEXT NOT NULL,
  last_read_id BIGINT NOT NULL,

  FOREIGN KEY ( user_id ) REFERENCES users ( id ) ON DELETE CASCADE,
  UNIQUE ( user_id, timeline )
)|}

let down (c : Sqlx.Connection.t) = c#execute {|DROP TABLE markers|}
