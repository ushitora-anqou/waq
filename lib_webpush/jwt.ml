open Util

let build ~aud ~exp ~sub ~priv_key =
  let header =
    `Assoc [ ("typ", `String "JWT"); ("alg", `String "ES256") ]
    |> Yojson.Safe.to_string |> b64_url_encode
  in
  let payload =
    `Assoc [ ("aud", `String aud); ("exp", `Int exp); ("sub", `String sub) ]
    |> Yojson.Safe.to_string |> b64_url_encode
  in
  let header_dot_payload = header ^ "." ^ payload in
  let signature =
    header_dot_payload |> Cstruct.of_string |> Mirage_crypto.Hash.SHA256.digest
    |> Mirage_crypto_ec.P256.Dsa.sign ~key:priv_key
    |> (fun (r, s) -> Cstruct.(concat [ r; s ] |> to_string))
    |> b64_url_encode
  in
  header_dot_payload ^ "." ^ signature
