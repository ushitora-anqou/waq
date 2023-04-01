open Entity
open Helper
open Lwt.Infix

let patch req =
  let%lwt self = authenticate_account req in

  (* FIXME: support more *)
  self#set_display_name
    (req
    |> Httpq.Server.query_opt "display_name"
    |> Option.value ~default:self#display_name);

  let%lwt a = Db.(e @@ Account.update [ self ]) >|= List.hd in
  Worker.Account_update.kick ~account_id:a#id ~updated_at:a#updated_at;%lwt
  make_credential_account_from_model a >|= yojson_of_account >>= respond_yojson
