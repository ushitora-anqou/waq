open Sqlx.Migration.Helper

let change =
  create_table ~table_name:"account_stats"
    ~schema:
      [
        {|account_id BIGINT NOT NULL|};
        {|statuses_count BIGINT NOT NULL|};
        {|following_count BIGINT NOT NULL|};
        {|followers_count BIGINT NOT NULL|};
        {|last_status_at TIMESTAMP WITHOUT TIME ZONE|};
        (* *)
        {|FOREIGN KEY (account_id) REFERENCES accounts(id) ON DELETE CASCADE|};
        {|UNIQUE (account_id)|};
      ]
