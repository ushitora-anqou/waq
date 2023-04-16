open Util

type t = { k : string; t : string }

let generate_key = Mirage_crypto_ec.P256.Dsa.generate

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

(*
let get_authorization_header_rfc8292 ~endpoint ~subscriber ~priv_key =
  (* See https://www.rfc-editor.org/rfc/rfc8292 *)
  build ~endpoint ~subscriber ~priv_key
  |> Result.map @@ fun { t; k } -> "vapid t=" ^ t ^ " k=" ^ k
*)
