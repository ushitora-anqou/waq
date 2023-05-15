open Sqlx.Migration.Helper

let change =
  create_table ~table_name:"users"
    ~schema:
      [
        "email TEXT NOT NULL";
        "encrypted_password TEXT NOT NULL";
        "account_id BIGINT NOT NULL";
        "UNIQUE (email)";
        (* *)
        "FOREIGN KEY (account_id) REFERENCES accounts (id) ON UPDATE CASCADE \
         ON DELETE CASCADE";
      ]
