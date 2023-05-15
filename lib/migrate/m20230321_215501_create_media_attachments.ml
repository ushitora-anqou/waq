open Sqlx.Migration.Helper

let change =
  create_table ~table_name:"media_attachments"
    ~schema:
      [
        {|status_id BIGINT|};
        {|account_id BIGINT|};
        {|remote_url TEXT NOT NULL|};
        {|type INTEGER NOT NULL|};
        (* *)
        {|FOREIGN KEY ( account_id ) REFERENCES accounts ( id ) ON DELETE SET NULL|};
        {|FOREIGN KEY ( status_id ) REFERENCES statuses ( id ) ON DELETE SET NULL|};
      ]
