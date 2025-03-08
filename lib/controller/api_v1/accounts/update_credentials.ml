open Entity
open Helper
open Util

let patch env req =
  let self = authenticate_account req in

  (* FIXME: support more *)
  req
  |> Yume.Server.query_opt "display_name"
  |> Option.value ~default:self#display_name
  |> self#set_display_name;
  req
  |> Yume.Server.query_opt "note"
  |> Option.value ~default:self#note
  |> self#set_note;
  ( req
  |> Yume.Server.query_opt "bot"
  |> Option.value ~default:(Model.Account.is_bot self |> string_of_bool)
  |> function
    | "true" -> self#set_actor_type (Some `Service)
    | "false" -> self#set_actor_type (Some `Person)
    | _ -> raise_error_response `Bad_request );

  (* Avatar *)
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o755
    Eio.Path.(env#fs / Config.account_avatar_dir ());
  Yume.Server.formdata "avatar" req
  |> Result.iter (fun formdata ->
         let _, file_name, _ =
           Lwt_eio.run_lwt @@ fun () ->
           Image.save_formdata ~outdir:(Config.account_avatar_dir ()) formdata
         in
         let file_url = Config.account_avatar_url file_name in
         self#set_avatar_remote_url (Some file_url));

  (* Header *)
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o755
    Eio.Path.(env#fs / Config.account_header_dir ());
  Yume.Server.formdata "header" req
  |> Result.iter (fun formdata ->
         let _, file_name, _ =
           Lwt_eio.run_lwt @@ fun () ->
           Image.save_formdata ~outdir:(Config.account_header_dir ()) formdata
         in
         let file_url = Config.account_header_url file_name in
         self#set_header_remote_url file_url);

  let a = Db.(e @@ Account.update [ self ]) |> List.hd in
  Worker.Account_update.kick env ~account_id:a#id ~updated_at:a#updated_at;
  make_credential_account_from_model a |> yojson_of_account |> respond_yojson
