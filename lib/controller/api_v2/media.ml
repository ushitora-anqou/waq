open Helper
open Lwt.Infix
open Util

let post _ req =
  let self = authenticate_account req in
  let formdata = Yume.Server.formdata_exn "file" req in

  let result_attachment = ref None in
  let success =
    Lwt_eio.run_lwt @@ fun () ->
    Db.transaction @@ fun c ->
    let%lwt attachment =
      Model.MediaAttachment.(
        save_one (make ~type_:0 ~remote_url:"" ~account_id:self#id ()) c)
    in

    (* Outdir is like static/system/media_attachments/files/110/319/951/869/694/611/ *)
    let id = Model.MediaAttachment.ID.to_int attachment#id in
    let original_outdir = K.original_media_attachments_dir id in
    let small_outdir = K.small_media_attachments_dir id in
    Lwt_unix.mkpath original_outdir 0o755;%lwt
    Lwt_unix.mkpath small_outdir 0o755;%lwt

    (* Convert input images *)
    let%lwt _file_type, file_name, original_file_path =
      Image.save_formdata ~outdir:original_outdir formdata
    in
    let%lwt _small_file_name, small_file_path =
      Image.save_thumbnail ~outdir:small_outdir ~file_name ~original_file_path
    in
    let blurhash =
      Image.(load_image_as_rgb24 ~path:small_file_path |> blurhash)
    in

    (* Update the attachment *)
    attachment#set_file_file_name (Some file_name);
    attachment#set_blurhash (Some blurhash);
    ( Model.MediaAttachment.save_one attachment c >|= fun a ->
      result_attachment := Some a );%lwt

    Lwt.return_unit
  in

  if not success then failwith "Transaction failed";
  let attachment = Option.get !result_attachment in

  attachment |> Entity.serialize_media_attachment
  |> Entity.yojson_of_media_attachment |> respond_yojson
