open Sqlx.Migration.Helper

let change =
  create_table ~table_name:"accounts"
    ~schema:
      [
        "username TEXT NOT NULL";
        "domain TEXT";
        "private_key TEXT";
        "public_key TEXT NOT NULL";
        "display_name TEXT NOT NULL";
        "uri TEXT NOT NULL";
        "url TEXT";
        "inbox_url TEXT NOT NULL";
        "outbox_url TEXT NOT NULL";
        "shared_inbox_url TEXT NOT NULL";
        "followers_url TEXT NOT NULL";
      ]
  *> create_index ~name:"index_accounts_20221230_220000" ~table_name:"accounts"
       ~schema:[ "LOWER(username)"; "COALESCE(LOWER(domain), '')" ]
