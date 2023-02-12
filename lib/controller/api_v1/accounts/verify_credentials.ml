open Entity
open Lwt.Infix
open Helper

let get req =
  let%lwt self_id = authenticate_user req in
  let%lwt a = Db.Account.get_one ~id:self_id () in
  make_credential_account_from_model a
  >|= credential_account_to_yojson >>= respond_yojson
