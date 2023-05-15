open Sqlx.Migration.Helper

let change =
  create_table ~table_name:"mentions"
    ~schema:
      [
        {|status_id BIGINT|};
        {|account_id BIGINT|};
        (* *)
        {|FOREIGN KEY ( account_id ) REFERENCES accounts ( id ) ON DELETE CASCADE|};
        {|FOREIGN KEY ( status_id ) REFERENCES statuses ( id ) ON DELETE CASCADE|};
      ]
