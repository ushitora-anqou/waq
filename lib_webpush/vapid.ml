open Util

type t = { k : string; t : string }

let generate_keys () =
  let open Mirage_crypto_ec.P256.Dsa in
  let priv_key, pub_key = generate () in
  ( priv_key |> priv_to_octets |> b64_url_encode,
    pub_key |> pub_to_octets |> b64_url_encode )

let build ~endpoint ~subscriber ~priv_key =
  let ( let* ) = Result.bind in

  (* Decode private key *)
  let* priv_key =
    priv_key |> b64_url_decode |> Mirage_crypto_ec.P256.Dsa.priv_of_octets
  in

  (* Generate JWT token *)
  let token =
    let aud =
      let u = Uri.of_string endpoint in
      let scheme = Uri.scheme u |> Option.get in
      let host = Uri.host u |> Option.get in
      scheme ^ "://" ^ host
    in
    let exp =
      let now = Unix.gettimeofday () |> int_of_float in
      now + (12 * 60 * 60)
    in
    Jwt.build ~aud ~exp ~sub:subscriber ~priv_key
  in

  (* Derive public key *)
  let pub_key =
    Mirage_crypto_ec.P256.Dsa.(
      pub_of_priv priv_key |> pub_to_octets |> b64_url_encode)
  in

  Ok { t = token; k = pub_key }

(*
let get_authorization_header_rfc8292 ~endpoint ~subscriber ~priv_key =
  (* See https://www.rfc-editor.org/rfc/rfc8292 *)
  build ~endpoint ~subscriber ~priv_key
  |> Result.map @@ fun { t; k } -> "vapid t=" ^ t ^ " k=" ^ k
*)
