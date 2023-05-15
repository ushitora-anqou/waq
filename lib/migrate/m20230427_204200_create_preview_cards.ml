open Sqlx.Migration.Helper

let change =
  create_table ~table_name:"preview_cards"
    ~schema:
      [
        {|url TEXT NOT NULL|};
        {|title TEXT NOT NULL|};
        {|description TEXT NOT NULL|};
        {|image_url TEXT|};
        {|type INT NOT NULL|};
        {|html TEXT NOT NULL|};
        {|author_name TEXT NOT NULL|};
        {|author_url TEXT NOT NULL|};
        {|provider_name TEXT NOT NULL|};
        {|provider_url TEXT NOT NULL|};
        {|width INT NOT NULL|};
        {|height INT NOT NULL|};
        {|embed_url TEXT NOT NULL|};
        (* *)
        {|UNIQUE ( url )|};
      ]
