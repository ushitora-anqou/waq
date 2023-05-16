let migrations : (int * (module Sqlx.Migration.S)) list =
  Migrate.
    [
#include "migrations.inc"
    ]
