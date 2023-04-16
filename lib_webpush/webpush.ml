module Vapid = Vapid
open Util

let construct_request ~message ~auth_key ~p256dh_key ~subscriber ~endpoint
    ~vapid_priv_key =
  let ( let* ) = Result.bind in
  let hmac_sha256 = Mirage_crypto.Hash.SHA256.hmac in

  let auth_secret = auth_key |> b64_url_decode |> Cstruct.of_string in
  let dh = p256dh_key |> b64_url_decode |> Cstruct.of_string in

  let salt = Mirage_crypto_rng.generate 16 in

  (* Derive shared secret by ECDH *)
  let local_priv_key, local_pub_key = Mirage_crypto_ec.P256.Dh.gen_key () in
  let* ecdh_secret = Mirage_crypto_ec.P256.Dh.key_exchange local_priv_key dh in

  (* Apply HKDF to combine auth_secret and ecdh_secret *)
  let prk_key = Mirage_crypto.Hash.SHA256.hmac ~key:auth_secret ecdh_secret in
  let key_info_0x01 =
    Cstruct.concat
      [
        Cstruct.of_string "WebPush: info\x00";
        dh;
        local_pub_key;
        Cstruct.of_string "\x01";
      ]
  in
  let ikm = hmac_sha256 ~key:prk_key key_info_0x01 in

  (* Derive content encryption key (CEK) and nonce by HKDF *)
  let prk = hmac_sha256 ~key:salt ikm in
  let cek_info_0x01 = Cstruct.of_string "Content-Encoding: aes128gcm\x00\x01" in
  let cek = Cstruct.sub (hmac_sha256 ~key:prk cek_info_0x01) 0 16 in
  let nonce_info_0x01 = Cstruct.of_string "Content-Encoding: nonce\x00\x01" in
  let nonce = Cstruct.sub (hmac_sha256 ~key:prk nonce_info_0x01) 0 12 in

  (* Build body *)
  let body =
    (* Figure of construction of the body:

        __________________ record_size ________________________
        ____________ record_length _________________ ___16 B___
        ___ length record ___
       +---------------------+----------+-----------+----------+
       |      header         |   data   |    pad    | auth tag |
       +---------------------+----------+-----------+----------+
    *)
    let record_size = 4096 in
    let record_length = record_size - 16 in
    let open Mirage_crypto.Cipher_block.AES.GCM in
    let key = of_secret cek in
    let open Cstruct in
    let header =
      (* Content-Coding Header:

         +-----------+--------+-----------+---------------+
         | salt (16) | rs (4) | idlen (1) | keyid (idlen) |
         +-----------+--------+-----------+---------------+
      *)
      let buf = create (length salt + 4 + 1 + length local_pub_key) in
      let off = 0 in
      blit salt 0 buf off (length salt);
      let off = off + length salt in
      BE.set_uint32 buf off (Int32.of_int record_size);
      let off = off + 4 in
      set_uint8 buf off (length local_pub_key);
      let off = off + 1 in
      blit local_pub_key 0 buf off (length local_pub_key);
      buf
    in
    let data =
      let buf = create (record_length - length header) (* Filled with 0 *) in
      let off = 0 in
      blit_from_string message 0 buf off (String.length message);
      let off = off + String.length message in
      set_uint8 buf off 2;
      authenticate_encrypt ~key ~nonce buf
    in
    concat [ header; data ]
  in

  let* auth_header, crypto_key =
    (* NOTE: Don't see RFC 8292 (https://www.rfc-editor.org/rfc/rfc8292),
       which Google Chrome doesn't support as of April 2023.
       See https://datatracker.ietf.org/doc/html/draft-ietf-webpush-vapid-01 instead. *)
    Vapid.(
      build ~endpoint ~subscriber ~priv_key:vapid_priv_key
      |> Result.map @@ fun { t; k } -> ("WebPush " ^ t, "p256ecdsa=" ^ k))
  in

  let headers =
    [
      (`Content_encoding, "aes128gcm");
      (`Content_type, "application/octest-stream");
      (`TTL, "86400");
      (`Crypto_key, crypto_key);
      (`Authorization, auth_header);
    ]
  in

  Ok (headers, body)
