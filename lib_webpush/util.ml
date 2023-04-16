let b64_url_encode str =
  let r = Base64.encode ~pad:false ~alphabet:Base64.uri_safe_alphabet str in
  match r with
  | Ok s -> s
  | Error _ ->
      failwith
        (Printf.sprintf "Something wrong happened while encoding\n  %s" str)

let b64_url_decode str =
  let r = Base64.decode ~pad:false ~alphabet:Base64.uri_safe_alphabet str in
  match r with
  | Ok s -> s
  | Error _ ->
      failwith
        (Printf.sprintf "Something wrong happened while decoding\n  %s" str)
