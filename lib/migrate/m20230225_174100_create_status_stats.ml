let up c =
  Sql.execute c
    {|
CREATE TABLE status_stats (
  id SERIAL PRIMARY KEY,
  status_id BIGINT NOT NULL,
  replies_count BIGINT NOT NULL,
  reblogs_count BIGINT NOT NULL,
  favourites_count BIGINT NOT NULL,
  created_at TIMESTAMP WITHOUT TIME ZONE NOT NULL,
  updated_at TIMESTAMP WITHOUT TIME ZONE NOT NULL,

  FOREIGN KEY (status_id) REFERENCES statuses(id) ON DELETE CASCADE,
  UNIQUE (status_id)
)|}

let down c = Sql.execute c {|DROP TABLE status_stats|}
