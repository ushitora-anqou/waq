open Util
open Lwt.Infix

module Magick = struct
  let convert_exe = "/usr/bin/convert"

  let convert ~(input_type : [ `PNG | `JPEG ])
      ~(input_data : string Lwt_stream.t) ~(output_file_name : string) =
    Lwt_io.with_temp_file
      ~suffix:(match input_type with `PNG -> ".png" | `JPEG -> ".jpeg")
    @@ fun (temp_file_name, oc) ->
    Lwt_io.write_stream oc input_data;%lwt
    Lwt_io.flush oc;%lwt
    let com =
      ( convert_exe,
        [| convert_exe; temp_file_name; "-strip"; output_file_name |] )
    in
    Lwt_process.exec com >|= function
    | WEXITED 0 -> ()
    | _ -> failwith "ImageMagick.convert: failed to convert"
end

let parse_content_type = function
  | "image/png" -> Ok `PNG
  | "image/jpeg" -> Ok `JPEG
  | _ -> Error "Invalid content type"

let generate_unique_filename img_type =
  Uuidm.(v `V4 |> to_string)
  ^ match img_type with `PNG -> ".png" | `JPEG -> ".jpeg"

let save_formdata ~name ~req ~outdir =
  let%lwt fdata = Httpq.Server.formdata name req in
  let input_type =
    match
      Multipart_form.Content_type.to_string fdata.content_type
      |> parse_content_type
    with
    | Ok t -> t
    | Error _ -> Httpq.Server.raise_error_response `Bad_request
  in
  let file_name = generate_unique_filename input_type in
  let file_path = Filename.concat outdir file_name in
  Magick.convert ~input_type
    ~input_data:(Lwt_stream.of_list [ fdata.content ])
    ~output_file_name:file_path;%lwt
  Lwt.return (file_name, file_path)
