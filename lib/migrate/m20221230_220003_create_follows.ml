open Sqlx.Migration.Helper

let change =
  create_table ~table_name:"follows"
    ~schema:
      [
        {|account_id BIGINT|};
        {|target_account_id BIGINT|};
        {|uri TEXT|};
        (* *)
        {|FOREIGN KEY (account_id) REFERENCES accounts (id) ON UPDATE CASCADE ON DELETE CASCADE|};
        {|FOREIGN KEY (target_account_id) REFERENCES accounts (id) ON UPDATE CASCADE ON DELETE CASCADE|};
        {|UNIQUE (account_id, target_account_id)|};
      ]
