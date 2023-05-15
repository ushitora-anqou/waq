open Sqlx.Migration.Helper

let change =
  create_table ~table_name:"notifications"
    ~schema:
      [
        {|activity_id BIGINT NOT NULL|};
        {|activity_type TEXT NOT NULL|};
        {|account_id BIGINT NOT NULL|};
        {|from_account_id BIGINT NOT NULL|};
        {|type TEXT NOT NULL|};
        (* *)
        {|FOREIGN KEY (account_id) REFERENCES accounts (id) ON DELETE CASCADE|};
        {|FOREIGN KEY (from_account_id) REFERENCES accounts (id) ON DELETE CASCADE|};
      ]
