open Sqlx.Migration.Helper

let change =
  create_table ~table_name:"favourites"
    ~schema:
      [
        {|account_id BIGINT NOT NULL|};
        {|status_id BIGINT NOT NULL|};
        (* *)
        {|FOREIGN KEY (account_id) REFERENCES accounts(id) ON DELETE CASCADE|};
        {|FOREIGN KEY (status_id) REFERENCES statuses(id) ON DELETE CASCADE|};
        {|UNIQUE (account_id, status_id)|};
      ]
