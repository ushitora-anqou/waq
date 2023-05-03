open Helper
open Lwt.Infix
open Util

let post req =
  let%lwt self = authenticate_account req in

  (* Retrieve inputs *)
  let%lwt file_name, _file_path =
    Httpq.Server.formdata_exn "file" req
    >>= Image.save_formdata ~outdir:(Config.media_attachment_dir ())
  in

  (* Determine output paths *)
  let file_url = Config.media_attachment_url file_name in

  (* Make records *)
  let%lwt attachment =
    Db.(
      e
        MediaAttachment.(
          make ~type_:0 ~remote_url:file_url ~account_id:self#id () |> save_one))
  in

  attachment |> Entity.serialize_media_attachment
  |> Entity.yojson_of_media_attachment |> respond_yojson
