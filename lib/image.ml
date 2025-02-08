open Util
open Lwt.Infix

module Magick = struct
  let convert_exe =
    Sys.getenv_opt "IMAGEMAGICK_CONVERT_PATH"
    |> Option.value ~default:"/usr/bin/convert"

  let convert' ~(input_file_name : string) ~(output_file_name : string)
      ~(options : string array) =
    let com =
      ( convert_exe,
        Array.concat
          [
            [| convert_exe; input_file_name |]; options; [| output_file_name |];
          ] )
    in
    Lwt_process.exec com >|= function
    | WEXITED 0 -> ()
    | _ -> failwith "ImageMagick.convert: failed to convert"

  type input =
    | InFile of string
    | InStream of ([ `PNG | `JPEG ] * string Lwt_stream.t)

  type output = OutFile of string

  let convert (i : input) (o : output) ~(options : string array) =
    let output_file_name = match o with OutFile s -> s in
    match i with
    | InFile input_file_name ->
        convert' ~input_file_name ~output_file_name ~options
    | InStream (input_type, input_data) ->
        Lwt_io.with_temp_file
          ~suffix:(match input_type with `PNG -> ".png" | `JPEG -> ".jpeg")
          (fun (temp_file_name, oc) ->
            Lwt_io.write_stream oc input_data;%lwt
            Lwt_io.flush oc;%lwt
            convert' ~input_file_name:temp_file_name ~output_file_name ~options)
end

let parse_content_type = function
  | "image/png" -> Ok `PNG
  | "image/jpeg" -> Ok `JPEG
  | _ -> Error "Invalid content type"

let generate_unique_filename img_type =
  generate_uuid_v4 () ^ match img_type with `PNG -> ".png" | `JPEG -> ".jpeg"

let save_formdata ~outdir (formdata : Yume.Server.formdata_t) =
  let fdata = formdata in
  let input_type =
    match
      Multipart_form.Content_type.to_string fdata.content_type
      |> parse_content_type
    with
    | Ok t -> t
    | Error _ -> Yume.Server.raise_error_response `Bad_request
  in
  let file_name = generate_unique_filename input_type in
  let file_path = Filename.concat outdir file_name in
  Magick.convert ~options:[| "-strip" |]
    (InStream (input_type, Lwt_stream.of_list [ fdata.content ]))
    (OutFile file_path)
  >|= fun () -> (input_type, file_name, file_path)

let save_thumbnail ~outdir ~file_name ~original_file_path =
  let file_path = Filename.concat outdir file_name in
  Magick.convert ~options:[| "-resize"; "230400@" |] (InFile original_file_path)
    (OutFile file_path)
  >|= fun () -> (file_name, file_path)

let load_image_as_rgb24 ~path =
  match OImages.(load path [] |> tag) with
  | Rgb24 img -> img
  | Rgba32 img -> img#to_rgb24
  | Index8 img -> img#to_rgb24
  | Index16 img -> img#to_rgb24
  | Cmyk32 _ -> failwith "Not supported image type: Cmyk32"

let blurhash (src : OImages.rgb24_class) =
  Blurhash.blur_hash_for_pixels ~x_components:3 ~y_components:3 ~width:src#width
    ~height:src#height ~bytes_per_row:(src#width * 3) src#dump

let dummy_blurhash = "LEHLk~WB2yk8pyo0adR*.7kCMdnj"

let inspect bin =
  Lwt_io.with_temp_file (fun (path, oc) ->
      Lwt_io.write oc bin;%lwt
      Lwt_io.flush oc;%lwt
      Lwt_io.close oc;%lwt
      let src = load_image_as_rgb24 ~path in
      let width = src#width in
      let height = src#height in
      let blurhash = blurhash src in
      Lwt.return (width, height, blurhash))
