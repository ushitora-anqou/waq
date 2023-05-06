open Util

let absolute_url (src : string) =
  let open Uri in
  let u = of_string src in
  let u = with_scheme u (Some "https") in
  let u = with_host u (Some (Config.server_name ())) in
  to_string u

let list_to_url = String.concat "/" *> absolute_url

let list_to_file_path x =
  List.fold_left Filename.concat (Config.static_root ()) x

let int_to_3digits i =
  let s = Printf.sprintf "%012d" i in
  String.[ sub s 0 3; sub s 3 3; sub s 6 3; sub s 9 3 ]

let media_attachments_prefix = [ "system"; "media_attachments"; "files" ]

let media_attachments_list key (id, file_name) =
  media_attachments_prefix @ int_to_3digits id @ [ key; file_name ]

let original_media_attachments_url =
  media_attachments_list "original" *> list_to_url

let small_media_attachments_url = media_attachments_list "small" *> list_to_url

let original_media_attachments_file_path =
  media_attachments_list "original" *> list_to_file_path

let small_media_attachments_file_path =
  media_attachments_list "small" *> list_to_file_path

let original_media_attachments_dir id =
  original_media_attachments_file_path (id, "x") |> Filename.dirname

let small_media_attachments_dir id =
  small_media_attachments_file_path (id, "x") |> Filename.dirname
