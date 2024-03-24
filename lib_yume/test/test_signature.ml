open Yume

let test_build_signing_string () =
  let signed_headers =
    "(request-target) host date" |> String.split_on_char ' '
  in
  let headers =
    [
      (`Host, "example.com");
      (`Date, "Sun, 05 Jan 2014 21:31:40 GMT");
      (`Content_type, "application/json");
      (`Digest, "SHA-256=X48E9qOokqqrvdts8nOJRJN3OWDUoyWxBf7kbu9DBPE=");
      (`Content_length, "18");
    ]
  in
  let meth = `POST in
  let path = "/foo?param=value&pet=dog" in
  assert (
    Signature.build_signing_string ~signed_headers ~headers ~meth ~path
    = String.trim
        {|
(request-target): post /foo?param=value&pet=dog
host: example.com
date: Sun, 05 Jan 2014 21:31:40 GMT
|})

let test_verify () =
  let pub_key =
    X509.Public_key.decode_pem
    @@ Cstruct.of_string
         {|
-----BEGIN PUBLIC KEY-----
MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEA4XhCcviDkcNrTyJHrqZZ
xO7huHxJ/RopvHLA3sa8a7LB121TVfP0id861XCSu1Tr5MIkhjjsIsEhBfqIm+bn
GxnZtD47XpAM7fvspFbgm9sWjG6XKxDRkxmpn61gwU75tn9W9pm9DwCmeyj2OnB3
LH0qePxLndozbLExqlbUXAaK+1ebMb1X2N4n5qJE4soBK7loK5P8YVT2Jfhi4diY
XLm9/V7pLtrZwchdQ8i0qscMpd0Of12soXXuYCYXEXv/zO0eqmroET6avzrIdp8P
n5kP8scEXuNyji8cqYXGoK6JyeMXhLoI/bfy3pK4inNu8hSBOmCcKpTejrRChCdk
QwIDAQAB
-----END PUBLIC KEY-----
 |}
    |> Result.get_ok
  in
  let headers =
    [
      (`Digest, "SHA-256=EJuNLu/GVnfYCOqSt0yi4SnnM2/TC5YuvA+ga0rvDIA=");
      (`Date, "Tue, 27 Dec 2022 11:33:08 GMT");
      (`Content_type, "application/activity+json");
      (`Accept_encoding, "gzip");
      (`Content_length, "238");
      (`User_agent, "http.rb/5.1.0 (Mastodon/4.0.2; +http://localhost:3000/)");
      (`Host, "c5ab-220-153-158-42.ngrok.io");
    ]
  in
  let meth = `POST in
  let path = "/users/anqou/inbox" in
  let body =
    Some
      {|{"@context":"https://www.w3.org/ns/activitystreams","id":"http://localhost:3000/388ac00f-5be9-43fd-b661-b5a6c39014a0","type":"Follow","actor":"http://localhost:3000/users/admin","object":"https://c5ab-220-153-158-42.ngrok.io/users/anqou"}|}
  in
  let { Signature.key_id; algorithm; headers = signed_headers; signature } =
    Signature.parse_signature_header
      {|keyId="http://localhost:3000/users/admin#main-key",algorithm="rsa-sha256",headers="(request-target) host date digest content-type",signature="rBIJyKvBjWlEsfhrqsxfZBWg3Vqwck5MHl9ohlG33mb8kUiZcTx7j7NLljXj8KIW4Gli61Jxmyu1EynRbWmYcvEm2ONa38+GzW9Pqnlm3Bli7yNzp4ga3BC1hrWh6V4sc8c7nHJix8qXKirmfmwtk9VNCxkWvwVRwJY5KHO/6ANDft81njlegvbnRdptbnubNOh8cmfz0y8vWyAMxSCmbKbc6M80WECw4Z+uHRh1LpunLerwAaNQNNLJ+MwjzyPznRkcKwem/RpZaUgIFNxH7N7HH87jp/737xwDlwZZQsiONhvGfn88rNZyZVUSnuZtom4oJQqKho6mmwTr2QhwJA=="|}
  in
  assert (key_id = "http://localhost:3000/users/admin#main-key");
  assert (algorithm = "rsa-sha256");
  assert (
    signed_headers
    = [ "(request-target)"; "host"; "date"; "digest"; "content-type" ]);
  assert (
    signature
    = "rBIJyKvBjWlEsfhrqsxfZBWg3Vqwck5MHl9ohlG33mb8kUiZcTx7j7NLljXj8KIW4Gli61Jxmyu1EynRbWmYcvEm2ONa38+GzW9Pqnlm3Bli7yNzp4ga3BC1hrWh6V4sc8c7nHJix8qXKirmfmwtk9VNCxkWvwVRwJY5KHO/6ANDft81njlegvbnRdptbnubNOh8cmfz0y8vWyAMxSCmbKbc6M80WECw4Z+uHRh1LpunLerwAaNQNNLJ+MwjzyPznRkcKwem/RpZaUgIFNxH7N7HH87jp/737xwDlwZZQsiONhvGfn88rNZyZVUSnuZtom4oJQqKho6mmwTr2QhwJA==");
  assert (
    Result.is_ok
    @@ Signature.verify ~pub_key ~algorithm ~signed_headers ~signature ~headers
         ~meth ~path ~body);
  ()

let test_sign () =
  let pub_key =
    X509.Public_key.decode_pem
    @@ Cstruct.of_string
         {|
-----BEGIN PUBLIC KEY-----
MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEA4XhCcviDkcNrTyJHrqZZ
xO7huHxJ/RopvHLA3sa8a7LB121TVfP0id861XCSu1Tr5MIkhjjsIsEhBfqIm+bn
GxnZtD47XpAM7fvspFbgm9sWjG6XKxDRkxmpn61gwU75tn9W9pm9DwCmeyj2OnB3
LH0qePxLndozbLExqlbUXAaK+1ebMb1X2N4n5qJE4soBK7loK5P8YVT2Jfhi4diY
XLm9/V7pLtrZwchdQ8i0qscMpd0Of12soXXuYCYXEXv/zO0eqmroET6avzrIdp8P
n5kP8scEXuNyji8cqYXGoK6JyeMXhLoI/bfy3pK4inNu8hSBOmCcKpTejrRChCdk
QwIDAQAB
-----END PUBLIC KEY-----
 |}
    |> Result.get_ok
  in
  let priv_key =
    X509.Private_key.decode_pem
    @@ Cstruct.of_string
         {|-----BEGIN RSA PRIVATE KEY-----
MIIEpQIBAAKCAQEA4XhCcviDkcNrTyJHrqZZxO7huHxJ/RopvHLA3sa8a7LB121T
VfP0id861XCSu1Tr5MIkhjjsIsEhBfqIm+bnGxnZtD47XpAM7fvspFbgm9sWjG6X
KxDRkxmpn61gwU75tn9W9pm9DwCmeyj2OnB3LH0qePxLndozbLExqlbUXAaK+1eb
Mb1X2N4n5qJE4soBK7loK5P8YVT2Jfhi4diYXLm9/V7pLtrZwchdQ8i0qscMpd0O
f12soXXuYCYXEXv/zO0eqmroET6avzrIdp8Pn5kP8scEXuNyji8cqYXGoK6JyeMX
hLoI/bfy3pK4inNu8hSBOmCcKpTejrRChCdkQwIDAQABAoIBAQCcg9Ts5qK+rv9x
AYTtMkEiugkq/eyGrIDIdoh4nVDzGjYBi7zL7/uTMckvnWPecwcF19du7VjOj26T
LeVjBBjt8TX1LzYbYLf5BGazdjOZPa8bcTGzDYvOJ4ReegaualpiW2dM7FAXeAYZ
fMfgrW2tDn5LWihLvcrw6thlPL1R21MzRn6uFzPk+EgQgGtQE4c9ATjnHSHwqlx0
0FT+oNIndeYltANqWZfvDlnazNs5pFgSal6kxJPhITh8EDBEotUWJ/DYv+rst5Gz
OvXYx+9uKzOnO+8tr8cM6/oA4iDGgZr0BCSeQCv2DyzWSpbVc6JM1vA1mOZWV4DC
6191nsaRAoGBAPcZbNjRKFgA81Wovg8j+GybBXEMbHyqBdBAWBNdaMLLxxJYpNil
PbMbdi1b2huJfqrXclVfmg9E437P9yhMs5fSu9XxsY8sHNqzhpx+FD7N4sBKaEVy
TA0IPofWDLBJlblIuC+bTvmZ12XnXIeqGA/G6aZpCUQ3EOs9et29aTeHAoGBAOmX
YcjHEzFze8f8GLNihJrxgmEzwrYY/oQzaTjkE4uOigrHGKlPy5K1i+HoXe+mVSsh
BTze93WUXUMAIBkGD/wQ8Vq5dmXM0eYox/B7gkuI5MrJrHcXLutdp7R6uKzzuE0d
EPj7dEx0TUuU4v5J1DW3dKdzTkunlYMZVb2t3qRlAoGBALSIhPgcSIeTmjV5ZfMV
6skesekYlKK4birq9+6eDo5T6AxFa5tFj0CuzbbyJvZ7jGoxor0xbhO91yCtvPxa
ndRNCSOUxI/AQQygobbwfsYsEoE/W9NxYJMc9OK1xut9t8B5muhdZmQCvFcdpIMS
CSZtxAPrNfP674ePkZc0bg09AoGBAJUpNqbYAHiObtdoMnmyCk1KtN/PBqHzWesf
TSyn75cpIRr6wdO0AaGgTj+mev2CMFaHEmopUsmBQQB8vwJbra9fPk+ZqHIfFVWs
aug0ZiJHuxmHKJsvcaWYO3Py/aRrSR/s1J65Ky17ArSl0UFh8aGPkW0wIyoKTF3X
urmRol9FAoGAQp0KcLfTOxyTpu7hW9DNKGK6dQKgIemt/FQIiSYnBisgupcAb2iq
KuuQLp5yZh1etL3Q+WGsZyK6ueIjX+34l0IZr5YEOFwAUxxY71LD7gDP4ikUVKWg
KgbztieZwDBihVKbPtiaiGxeNXrxGWfL37BB0Jcy/RRYomLBjwTj2Ks=
-----END RSA PRIVATE KEY-----
|}
    |> Result.get_ok
  in
  let signed_headers =
    "(request-target) host date digest content-type" |> String.split_on_char ' '
  in
  let headers =
    [
      (`Date, "Tue, 27 Dec 2022 11:33:08 GMT");
      (`Content_type, "application/activity+json");
      (`Accept_encoding, "gzip");
      (`Content_length, "238");
      (`User_agent, "http.rb/5.1.0 (Mastodon/4.0.2; +http://localhost:3000/)");
      (`Host, "c5ab-220-153-158-42.ngrok.io");
    ]
  in
  let meth = `POST in
  let path = "/users/anqou/inbox" in
  let key_id = "http://localhost:3000/users/admin#main-key" in
  let body =
    Some
      {|{"@context":"https://www.w3.org/ns/activitystreams","id":"http://localhost:3000/388ac00f-5be9-43fd-b661-b5a6c39014a0","type":"Follow","actor":"http://localhost:3000/users/admin","object":"https://c5ab-220-153-158-42.ngrok.io/users/anqou"}|}
  in
  let new_headers =
    Signature.sign ~priv_key ~key_id ~signed_headers ~headers ~meth ~path ~body
  in
  assert (
    List.combine
      ((`Signature, "") :: (`Digest, "") :: headers |> List.sort compare)
      (new_headers |> List.sort compare)
    |> List.for_all (fun ((k, v), (k', v')) ->
           k = k' && (k = `Signature || k = `Digest || v = v')));
  let {
    Signature.key_id = key_id';
    algorithm;
    headers = signed_headers;
    signature;
  } =
    List.assoc `Signature new_headers |> Signature.parse_signature_header
  in
  assert (key_id = key_id');
  assert (
    Result.is_ok
    @@ Signature.verify ~pub_key ~algorithm ~signed_headers ~signature ~headers
         ~meth ~path ~body)

let () =
  let open Alcotest in
  Mirage_crypto_rng_unix.initialize (module Mirage_crypto_rng.Fortuna);
  run "signature"
    [
      ( "build_signing_string",
        [ test_case "case1" `Quick test_build_signing_string ] );
      ("sign", [ test_case "case1" `Quick test_sign ]);
      ("verify", [ test_case "case1" `Quick test_verify ]);
    ]
