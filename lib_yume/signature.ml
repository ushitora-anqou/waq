type private_key = X509.Private_key.t
type public_key = X509.Public_key.t
type keypair = private_key * public_key

let encode_public_key (pub_key : public_key) : string =
  X509.Public_key.encode_pem pub_key |> Cstruct.to_string

let encode_private_key (priv_key : private_key) : string =
  X509.Private_key.encode_pem priv_key |> Cstruct.to_string

let decode_private_key (src : string) : private_key =
  src |> Cstruct.of_string |> X509.Private_key.decode_pem |> Result.get_ok

let decode_public_key (src : string) : public_key =
  src |> Cstruct.of_string |> X509.Public_key.decode_pem |> Result.get_ok

type signature_header = {
  key_id : string;
  signature : string;
  algorithm : string;
  headers : string list;
}
[@@deriving make]

let string_of_signature_header (p : signature_header) : string =
  [
    ("keyId", p.key_id);
    ("algorithm", p.algorithm);
    ("headers", p.headers |> String.concat " ");
    ("signature", p.signature);
  ]
  |> List.map (fun (k, v) -> k ^ "=\"" ^ v ^ "\"" (* FIXME: escape? *))
  |> String.concat ","

let generate_keypair () : keypair =
  let priv = X509.Private_key.generate ~bits:2048 `RSA in
  let pub = X509.Private_key.public priv in
  (priv, pub)

let build_signing_string ~(signed_headers : string list) ~(headers : Headers.t)
    ~(meth : Method.t) ~(path : string) : string =
  let pseudo_headers =
    headers |> List.map (fun (k, v) -> (k |> Header.lower_string_of_name, v))
  in
  let meth = Method.to_string meth |> String.lowercase_ascii in
  signed_headers
  |> List.map (function
       | "(request-target)" -> "(request-target): " ^ meth ^ " " ^ path
       | "(created)" | "(expires)" -> failwith "Not implemented"
       | header ->
           let values =
             pseudo_headers
             |> List.filter_map (function
                  | k, v when k = header -> Some v
                  | _ -> None)
           in
           if List.length values = 0 then
             failwith ("Specified signed header not found: " ^ header)
           else
             let value = values |> String.concat ", " in
             header ^ ": " ^ value)
  |> String.concat "\n"

let may_cons_digest_header ?(prefix = "SHA-256") (headers : Headers.t)
    (body : string option) : Headers.t =
  body
  |> Option.fold ~none:headers ~some:(fun body ->
         let digest =
           body |> Cstruct.of_string |> Mirage_crypto.Hash.SHA256.digest
           |> Cstruct.to_string |> Base64.encode_exn
         in
         let digest = prefix ^ "=" ^ digest in
         match List.assoc_opt `Digest headers with
         | Some v when v <> digest -> failwith "Digest not match"
         | Some _ -> headers
         | _ -> headers |> List.cons (`Digest, digest))

let sign ~(priv_key : private_key) ~(key_id : string)
    ~(signed_headers : string list) ~(headers : Headers.t) ~(meth : Method.t)
    ~(path : string) ~(body : string option) : Headers.t =
  let algorithm = "rsa-sha256" in
  let headers = may_cons_digest_header headers body in
  let signing_string =
    build_signing_string ~signed_headers ~headers ~meth ~path
  in
  let signature =
    match
      X509.Private_key.sign `SHA256 priv_key ~scheme:`RSA_PKCS1
        (`Message (Cstruct.of_string signing_string))
    with
    | Ok s -> Cstruct.to_string s |> Base64.encode_exn
    | Error (`Msg s) -> failwith ("Sign error: " ^ s)
  in
  let sig_header =
    make_signature_header ~key_id ~signature ~algorithm ~headers:signed_headers
      ()
    |> string_of_signature_header
  in
  headers |> List.cons (`Signature, sig_header)

let parse_signature_header (src : string) : signature_header =
  let fields =
    src |> String.split_on_char ','
    |> List.map (fun s ->
           let pos = String.index s '=' in
           ( String.sub s 0 pos,
             String.sub s (pos + 1) (String.length s - (pos + 1)) ))
    |> List.map (fun (k, v) -> (k, String.sub v 1 (String.length v - 2)))
  in
  let key_id = List.assoc "keyId" fields in
  let signature = List.assoc "signature" fields in
  let algorithm = List.assoc "algorithm" fields in
  let headers = List.assoc "headers" fields in
  let headers = String.split_on_char ' ' headers in
  make_signature_header ~key_id ~signature ~algorithm ~headers ()

let verify ~(pub_key : public_key) ~(algorithm : string)
    ~(signed_headers : string list) ~(signature : string) ~(headers : Headers.t)
    ~(meth : Method.t) ~(path : string) ~(body : string option) : _ result =
  if algorithm <> "rsa-sha256" then Error `AlgorithmNotImplemented
  else
    let headers = may_cons_digest_header headers body in
    let signing_string =
      build_signing_string ~signed_headers ~headers ~meth ~path
    in
    X509.Public_key.verify `SHA256 ~scheme:`RSA_PKCS1
      ~signature:(signature |> Base64.decode_exn |> Cstruct.of_string)
      pub_key
      (`Message (Cstruct.of_string signing_string))
