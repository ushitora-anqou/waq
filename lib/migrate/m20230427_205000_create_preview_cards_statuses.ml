open Sqlx.Migration.Helper

let change =
  create_table ~table_name:"preview_cards_statuses"
    ~schema:
      [ {|preview_card_id BIGINT NOT NULL|}; {|status_id BIGINT NOT NULL|} ]
