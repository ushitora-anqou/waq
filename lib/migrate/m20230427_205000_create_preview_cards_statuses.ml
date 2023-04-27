let up (c : Sqlx.Connection.t) =
  c#execute
    {|
CREATE TABLE preview_cards_statuses (
  id SERIAL PRIMARY KEY,
  created_at TIMESTAMP WITHOUT TIME ZONE NOT NULL,
  updated_at TIMESTAMP WITHOUT TIME ZONE NOT NULL,

  preview_card_id BIGINT NOT NULL,
  status_id BIGINT NOT NULL
)|}

let down (c : Sqlx.Connection.t) =
  c#execute {|DROP TABLE preview_cards_statuses|}
