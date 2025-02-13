open Helper

let get_content_type = function
  | ".html" | ".htm" -> "text/html"
  | ".png" -> "image/png"
  | ".jpg" | ".jpeg" -> "image/jpeg"
  | _ -> raise_error_response `Not_found

let get_body path =
  (* FIXME: Use stream *)
  try
    let ic = open_in_bin path in
    let s = In_channel.input_all ic in
    close_in ic;
    s
  with _ -> raise_error_response `Not_found

let get _ req =
  let root = Config.static_root () |> Unix.realpath in
  let path =
    try Filename.concat root (Yume.Server.path req) |> Unix.realpath
    with Unix.Unix_error (Unix.ENOENT, "realpath", _) ->
      raise_error_response `Not_found
  in
  if not (String.starts_with ~prefix:root path) then
    raise_error_response `Not_found
  else
    Yume.Server.respond
      ~headers:[ (`Content_type, Filename.extension path |> get_content_type) ]
      (get_body path)
