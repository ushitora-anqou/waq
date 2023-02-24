let up c =
  Sql.execute c
    {|
CREATE TABLE accounts (
  id SERIAL PRIMARY KEY,
  username TEXT NOT NULL,
  domain TEXT,
  private_key TEXT,
  public_key TEXT NOT NULL,
  display_name TEXT NOT NULL,
  uri TEXT NOT NULL,
  url TEXT,
  inbox_url TEXT NOT NULL,
  outbox_url TEXT NOT NULL,
  shared_inbox_url TEXT NOT NULL,
  followers_url TEXT NOT NULL,
  created_at TIMESTAMP WITHOUT TIME ZONE NOT NULL,
  updated_at TIMESTAMP WITHOUT TIME ZONE NOT NULL
)|};%lwt
  Sql.execute c
    {|
CREATE INDEX ON accounts (
  LOWER(username),
  COALESCE(LOWER(domain), '')
)
|}

let down c = Sql.execute c {|DROP TABLE accounts|}
