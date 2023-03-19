open Helper

let get_content_type = function
  | ".html" | ".htm" -> "text/html"
  | ".png" -> "image/png"
  | _ -> raise_error_response `Not_found

let get_body path =
  (* FIXME: Use stream *)
  try
    let ic = open_in_bin path in
    let s = In_channel.input_all ic in
    close_in ic;
    s
  with _ -> raise_error_response `Not_found

let get req =
  let root = Config.static_root () |> Unix.realpath in
  let path = Filename.concat root (Httpq.Server.path req) |> Unix.realpath in
  if not (String.starts_with ~prefix:root path) then
    raise_error_response `Not_found
  else
    Httpq.Server.respond
      ~headers:[ (`Content_type, Filename.extension path |> get_content_type) ]
      (get_body path)
