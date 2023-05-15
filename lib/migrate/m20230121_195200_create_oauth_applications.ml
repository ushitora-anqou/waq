open Sqlx.Migration.Helper

let change =
  create_table ~table_name:"oauth_applications"
    ~schema:
      [
        {|name TEXT NOT NULL|};
        {|uid TEXT NOT NULL|};
        {|secret TEXT NOT NULL|};
        {|redirect_uri TEXT NOT NULL|};
        {|scopes TEXT NOT NULL|};
        (* *)
        {|UNIQUE (uid)|};
      ]
