open Sqlx.Migration.Helper

let change =
  create_table ~table_name:"status_stats"
    ~schema:
      [
        {|status_id BIGINT NOT NULL|};
        {|replies_count BIGINT NOT NULL|};
        {|reblogs_count BIGINT NOT NULL|};
        {|favourites_count BIGINT NOT NULL|};
        (* *)
        {|FOREIGN KEY (status_id) REFERENCES statuses(id) ON DELETE CASCADE|};
        {|UNIQUE (status_id)|};
      ]
