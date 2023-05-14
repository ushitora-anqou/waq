let up (c : Sqlx.Connection.t) =
  c#execute
    {|
CREATE TABLE preview_cards (
  id SERIAL PRIMARY KEY,
  created_at TIMESTAMP WITHOUT TIME ZONE NOT NULL,
  updated_at TIMESTAMP WITHOUT TIME ZONE NOT NULL,

  url TEXT NOT NULL,
  title TEXT NOT NULL,
  description TEXT NOT NULL,
  image_url TEXT,
  type INT NOT NULL,
  html TEXT NOT NULL,
  author_name TEXT NOT NULL,
  author_url TEXT NOT NULL,
  provider_name TEXT NOT NULL,
  provider_url TEXT NOT NULL,
  width INT NOT NULL,
  height INT NOT NULL,
  embed_url TEXT NOT NULL,

  UNIQUE ( url )
)|}

let down (c : Sqlx.Connection.t) = c#execute {|DROP TABLE preview_cards|}

open Sqlx.Migration.Helper

let change =
  create_table ~table_name:"preview_cards"
    ~schema:
      [
        {|url TEXT NOT NULL|};
        {|title TEXT NOT NULL|};
        {|description TEXT NOT NULL|};
        {|image_url TEXT|};
        {|type INT NOT NULL|};
        {|html TEXT NOT NULL|};
        {|author_name TEXT NOT NULL|};
        {|author_url TEXT NOT NULL|};
        {|provider_name TEXT NOT NULL|};
        {|provider_url TEXT NOT NULL|};
        {|width INT NOT NULL|};
        {|height INT NOT NULL|};
        {|embed_url TEXT NOT NULL|};
        (* *)
        {|UNIQUE ( url )|};
      ]
