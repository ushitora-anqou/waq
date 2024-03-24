open Entity
open Helper

let get _ req =
  let self = authenticate_account req in
  make_credential_account_from_model self |> yojson_of_account |> respond_yojson
