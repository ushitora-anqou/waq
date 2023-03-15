let up (c : Sqlx.Connection.t) =
  c#execute
    {|ALTER TABLE oauth_access_grants ADD COLUMN updated_at TIMESTAMP WITHOUT TIME ZONE NOT NULL DEFAULT now()|};%lwt
  c#execute
    {|ALTER TABLE oauth_access_tokens ADD COLUMN updated_at TIMESTAMP WITHOUT TIME ZONE NOT NULL DEFAULT now()|}

let down (c : Sqlx.Connection.t) =
  c#execute {|ALTER TABLE oauth_access_tokens DROP COLUMN updated_at|};%lwt
  c#execute {|ALTER TABLE oauth_access_grants DROP COLUMN updated_at|}
