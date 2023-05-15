open Sqlx.Migration.Helper

let change =
  add_column ~table_name:"oauth_access_grants" ~name:"updated_at"
    ~spec:"TIMESTAMP WITHOUT TIME ZONE NOT NULL DEFAULT now()"
  *> add_column ~table_name:"oauth_access_tokens" ~name:"updated_at"
       ~spec:"TIMESTAMP WITHOUT TIME ZONE NOT NULL DEFAULT now()"
