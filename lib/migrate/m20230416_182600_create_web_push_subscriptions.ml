open Sqlx.Migration.Helper

let change =
  create_table ~table_name:"web_push_subscriptions"
    ~schema:
      [
        {|endpoint TEXT NOT NULL|};
        {|key_p256dh TEXT NOT NULL|};
        {|key_auth TEXT NOT NULL|};
        {|access_token_id BIGINT|};
        {|user_id BIGINT|};
        (* *)
        {|FOREIGN KEY ( user_id ) REFERENCES users ( id ) ON DELETE CASCADE|};
        {|FOREIGN KEY ( access_token_id ) REFERENCES oauth_access_tokens ( id ) ON DELETE CASCADE|};
      ]
