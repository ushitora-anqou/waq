let up (c : Sqlx.Connection.t) =
  c#execute
    {|
CREATE TABLE oauth_access_tokens (
  id SERIAL PRIMARY KEY,
  token TEXT NOT NULL,
  created_at TIMESTAMP WITHOUT TIME ZONE NOT NULL,
  scopes TEXT,
  application_id BIGINT,
  resource_owner_id BIGINT,

  UNIQUE (token),
  FOREIGN KEY (resource_owner_id) REFERENCES users (id) ON DELETE CASCADE,
  FOREIGN KEY (application_id) REFERENCES oauth_applications (id) ON DELETE CASCADE
)|}

let down (c : Sqlx.Connection.t) = c#execute {|DROP TABLE oauth_access_tokens|}

open Sqlx.Migration.Helper

let change =
  create_table_not_model ~table_name:"oauth_access_tokens"
    ~schema:
      [
        {|id SERIAL PRIMARY KEY|};
        {|token TEXT NOT NULL|};
        {|created_at TIMESTAMP WITHOUT TIME ZONE NOT NULL|};
        {|scopes TEXT|};
        {|application_id BIGINT|};
        {|resource_owner_id BIGINT|};
        (* *)
        {|UNIQUE (token)|};
        {|FOREIGN KEY (resource_owner_id) REFERENCES users (id) ON DELETE CASCADE|};
        {|FOREIGN KEY (application_id) REFERENCES oauth_applications (id) ON DELETE CASCADE|};
      ]
