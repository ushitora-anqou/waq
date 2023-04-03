open Helper
open Lwt.Infix
open Util

let post req =
  let%lwt self = authenticate_account req in

  (* Retrieve inputs *)
  let fdata = Httpq.Server.formdata "file" req in
  let content_type =
    match Multipart_form.Content_type.to_string fdata.content_type with
    | "image/png" -> `PNG
    | "image/jpeg" -> `JPEG
    | _ -> raise_error_response `Bad_request
  in

  (* Determine output paths *)
  let file_name =
    Uuidm.(v `V4 |> to_string)
    ^ match content_type with `PNG -> ".png" | `JPEG -> ".jpeg"
  in
  let file_path = Config.media_attachment_path file_name in
  let file_url = Config.media_attachment_url file_name in

  (* Convert the media *)
  ImageMagick.convert ~input_type:content_type ~input_data:fdata.stream
    ~output_file_name:file_path;%lwt

  (* Make records *)
  let%lwt attachment =
    Db.(
      e
        MediaAttachment.(
          make ~type_:0 ~remote_url:file_url ~account_id:self#id () |> save_one))
  in

  attachment |> Entity.serialize_media_attachment
  |> Entity.yojson_of_media_attachment |> respond_yojson
