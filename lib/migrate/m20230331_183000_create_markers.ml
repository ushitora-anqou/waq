open Sqlx.Migration.Helper

let change =
  create_table ~table_name:"markers"
    ~schema:
      [
        {|user_id BIGINT|};
        {|timeline TEXT NOT NULL|};
        {|last_read_id BIGINT NOT NULL|};
        (* *)
        {|FOREIGN KEY ( user_id ) REFERENCES users ( id ) ON DELETE CASCADE|};
        {|UNIQUE ( user_id, timeline )|};
      ]
