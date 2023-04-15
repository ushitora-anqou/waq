type t = { k : string; t : string }

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

let build ~endpoint ~subscriber ~priv_key =
  let ( let* ) = Result.bind in

  (* Decode private key *)
  let* priv_key =
    priv_key |> b64_url_decode |> Cstruct.of_string
    |> Mirage_crypto_ec.P256.Dsa.priv_of_cstruct
  in

  (* Generate JWT token *)
  let aud_value =
    let u = Uri.of_string endpoint in
    let scheme = Uri.scheme u |> Option.get in
    let host = Uri.host u |> Option.get in
    scheme ^ "://" ^ host
  in
  let exp_value =
    let now = Unix.gettimeofday () |> int_of_float in
    now + (12 * 60 * 60) |> string_of_int
  in
  let token =
    Jwt.(
      empty_payload |> add_claim aud aud_value |> add_claim exp exp_value
      |> add_claim sub subscriber
      |> t_of_payload (`ES256 priv_key)
      |> token_of_t)
  in

  (* Derive public key *)
  let pub_key =
    Mirage_crypto_ec.P256.Dsa.(
      pub_of_priv priv_key |> pub_to_cstruct |> Cstruct.to_string
      |> b64_url_encode)
  in

  Ok { t = token; k = pub_key }

let get_authorization_header ~endpoint ~subscriber ~priv_key =
  build ~endpoint ~subscriber ~priv_key
  |> Result.map @@ fun { t; k } -> "vapid t=" ^ t ^ " k=" ^ k
