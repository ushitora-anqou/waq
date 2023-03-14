open Entity
open Helper
open Lwt.Infix

let patch req =
  let%lwt self_id = authenticate_user req in
  let%lwt a = Db.e (Model.Account.get_one ~id:self_id) in

  (* FIXME: support more *)
  a#set_display_name
    (req
    |> Httpq.Server.query_opt "display_name"
    |> Option.value ~default:a#display_name);

  let%lwt a = Db.(e @@ Account.update [ a ]) >|= List.hd in
  Worker.Account_update.kick ~account_id:a#id ~updated_at:a#updated_at;%lwt
  make_credential_account_from_model a >|= yojson_of_account >>= respond_yojson
