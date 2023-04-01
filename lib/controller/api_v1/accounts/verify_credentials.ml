open Entity
open Lwt.Infix
open Helper

let get req =
  let%lwt self = authenticate_account req in
  make_credential_account_from_model self
  >|= yojson_of_account >>= respond_yojson
