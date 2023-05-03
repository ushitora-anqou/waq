open Entity
open Helper
open Lwt.Infix

let patch req =
  let%lwt self = authenticate_account req in

  (* FIXME: support more *)
  req
  |> Httpq.Server.query_opt "display_name"
  >|= Option.value ~default:self#display_name
  >|= self#set_display_name;%lwt
  req
  |> Httpq.Server.query_opt "note"
  >|= Option.value ~default:self#note
  >|= self#set_note;%lwt

  (* Avatar *)
  Httpq.Server.formdata "avatar" req
  |> Lwt_result.iter (fun formdata ->
         let%lwt file_name, _file_path =
           Image.save_formdata ~outdir:(Config.account_avatar_dir ()) formdata
         in
         let file_url = Config.account_avatar_url file_name in
         self#set_avatar_remote_url (Some file_url);
         Lwt.return_unit);%lwt

  (* Header *)
  Httpq.Server.formdata "header" req
  |> Lwt_result.iter (fun formdata ->
         let%lwt file_name, _file_path =
           Image.save_formdata ~outdir:(Config.account_header_dir ()) formdata
         in
         let file_url = Config.account_header_url file_name in
         self#set_header_remote_url file_url;
         Lwt.return_unit);%lwt

  let%lwt a = Db.(e @@ Account.update [ self ]) >|= List.hd in
  Worker.Account_update.kick ~account_id:a#id ~updated_at:a#updated_at;%lwt
  make_credential_account_from_model a >|= yojson_of_account >>= respond_yojson
