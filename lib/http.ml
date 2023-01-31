open Util

module Uri = struct
  include Uri

  let getaddrinfo_port (u : t) =
    let scheme = Uri.scheme u |> Option.get in
    u |> Uri.port |> Option.fold ~none:scheme ~some:string_of_int

  let http_host (u : t) =
    let host = Uri.host u |> Option.get in
    match Uri.port u with
    | None -> host
    | Some port -> host ^ ":" ^ string_of_int port

  let path_query_fragment (u : t) =
    let res = Uri.path u in
    let res =
      match Uri.verbatim_query u with None -> res | Some q -> res ^ "?" ^ q
    in
    let res =
      match Uri.fragment u with None -> res | Some f -> res ^ "#" ^ f
    in
    res

  let domain (u : t) = http_host u
end

module Method = struct
  type t = Cohttp.Code.meth

  let to_string = Cohttp.Code.string_of_method
end

module Status = struct
  type t = Cohttp.Code.status_code

  let to_string = Cohttp.Code.string_of_status
end

module Signature = struct
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

  let build_signing_string ~(signed_headers : string list)
      ~(headers : (string * string) list) ~(meth : Method.t) ~(path : string) :
      string =
    let pseudo_headers =
      headers |> List.map (fun (k, v) -> (String.lowercase_ascii k, v))
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

  let may_cons_digest_header ?(prefix = "SHA-256")
      (headers : (string * string) list) (body : string option) :
      (string * string) list =
    body
    |> Option.fold ~none:headers ~some:(fun body ->
           let digest = Sha256.(string body |> to_bin |> Base64.encode_exn) in
           let digest = prefix ^ "=" ^ digest in
           match List.assoc_opt "Digest" headers with
           | Some v when v <> digest -> failwith "Digest not match"
           | Some _ -> headers
           | _ -> ("Digest", digest) :: headers)

  let sign ~(priv_key : private_key) ~(key_id : string)
      ~(signed_headers : string list) ~(headers : (string * string) list)
      ~(meth : Method.t) ~(path : string) ~(body : string option) :
      (string * string) list =
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
      make_signature_header ~key_id ~signature ~algorithm
        ~headers:signed_headers ()
      |> string_of_signature_header
    in
    ("Signature", sig_header) :: headers

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
      ~(signed_headers : string list) ~(signature : string)
      ~(headers : (string * string) list) ~(meth : Method.t) ~(path : string)
      ~(body : string option) : _ result =
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
end

type request = {
  req : Cohttp_lwt.Request.t;
  query : (string * string list) list;
  param : (string * string) list;
  body : Cohttp_lwt.Body.t;
  headers : (string * string) list;
}
[@@deriving make]

type response = Cohttp_lwt_unix.Server.response_action
type handler = request -> response Lwt.t
type path = [ `L of string | `P of string ] list
type method_ = Method.t
type route = method_ * path * handler

exception ErrorResponse of Status.t * string

let raise_error_response ?(body = "") status =
  raise (ErrorResponse (status, body))

let respond ?(status = `OK) ?(headers = []) (body : string) =
  let open Lwt.Infix in
  let headers = headers |> Cohttp.Header.of_list in
  Cohttp_lwt_unix.Server.respond_string ~status ~headers ~body () >|= fun x ->
  `Response x

let start_server ?(port = 8080) f (routes : route list) =
  let open Cohttp in
  let open Cohttp_lwt_unix in
  let callback _conn req body =
    let uri = Request.uri req in
    let meth = Request.meth req in
    let headers = Request.headers req |> Header.to_list in

    (* Parse target *)
    let path, query =
      (Uri.path uri |> String.split_on_char '/' |> List.tl, Uri.query uri)
    in

    (* Choose correct handler via router *)
    let param, handler =
      let default_handler =
        Fun.const
        @@ respond
             ~status:
               (match meth with `GET -> `Not_found | _ -> `Method_not_allowed)
             ""
      in
      let rec match_path param = function
        | [], [] -> Some param
        | x :: xs, `L y :: ys when x = y -> match_path param (xs, ys)
        | x :: xs, `P y :: ys -> match_path ((y, x) :: param) (xs, ys)
        | _ -> None
      in
      routes
      |> List.find_map (fun (meth', ptn, handler) ->
             if meth <> meth' then None
             else
               match_path [] (path, ptn)
               |> Option.map (fun param -> (param, handler)))
      |> Option.value ~default:([], default_handler)
    in

    (* Invoke the handler *)
    try%lwt
      let%lwt res =
        handler (make_request ~req ~query ~param ~body ~headers ())
      in
      (match res with
      | `Response res ->
          Log.info (fun m ->
              m "%s %s %s"
                (Status.to_string (fst res).status)
                (Method.to_string meth) (Uri.to_string uri))
      | `Expert _ ->
          Log.info (fun m ->
              m "Expert %s %s" (Method.to_string meth) (Uri.to_string uri)));
      Lwt.return res
    with
    | ErrorResponse (status, body) -> respond ~status body
    | e ->
        Log.err (fun m ->
            m "Unexpected exception: %s %s: %s\n%s" (Method.to_string meth)
              (Uri.to_string uri) (Printexc.to_string e)
              (Printexc.get_backtrace ()));
        respond ~status:`Internal_server_error ""
  in
  let server =
    Server.make_response_action ~callback ()
    |> Server.create ~mode:(`TCP (`Port port))
  in
  Lwt.join [ server; f () ] |> Lwt_main.run

let router (routes : route list) = routes

let parse_path (src : string) : path =
  src |> String.split_on_char '/' |> List.tl
  |> List.map (function
       | x when String.starts_with ~prefix:":" x -> `P x
       | x -> `L x)

let call (meth : Method.t) (path : string) (h : handler) =
  let path = parse_path path in
  (meth, path, h)

let query_opt (k : string) (r : request) : string list option =
  List.assoc_opt k r.query

let query (k : string) (r : request) : string list = query_opt k r |> Option.get
let param (k : string) (r : request) : string = List.assoc k r.param
let body (r : request) : string Lwt.t = Cohttp_lwt.Body.to_string r.body
let headers (r : request) : (string * string) list = r.headers

type ws_conn = {
  mutable frames_out_fn : (Websocket.Frame.t option -> unit) option;
  mutable closed : bool; [@default false]
  recv_stream : string Lwt_stream.t;
}
[@@deriving make]

let ws_send ?(opcode = Websocket.Frame.Opcode.Text) (c : ws_conn) content =
  if not c.closed then
    Websocket.Frame.create ~opcode ~content ()
    |> Option.some |> Option.get c.frames_out_fn

let ws_recv (c : ws_conn) = Lwt_stream.get c.recv_stream

let websocket (r : request) f =
  Cohttp_lwt.Body.drain_body r.body;%lwt
  let recv_stream, recv_stream_push = Lwt_stream.create () in
  let conn = make_ws_conn ~recv_stream () in
  let%lwt resp, frames_out_fn =
    Websocket_cohttp_lwt.upgrade_connection r.req (fun { opcode; content; _ } ->
        match opcode with
        | Close ->
            Log.debug (fun m -> m "Websocket: recv Close");
            ws_send ~opcode:Close conn "";
            conn.closed <- true;
            recv_stream_push None
        | Text | Binary ->
            Log.debug (fun m -> m "Websocket: recv: %s" content);
            recv_stream_push (Some content)
        | Ping -> ws_send ~opcode:Pong conn content
        | Pong -> () (* Just ignore *)
        | _ ->
            Websocket.Frame.close 1002
            |> Option.(some |$> get conn.frames_out_fn))
  in
  conn.frames_out_fn <- Some frames_out_fn;
  Lwt.async (fun () ->
      try
        Log.debug (fun m -> m "Websocket: start thread");
        f conn
      with e ->
        Log.err (fun m ->
            m "Websocket: thread error: %s" (Printexc.to_string e));
        Lwt.return_unit);
  Lwt.return resp

let fetch ?(headers = []) ?(meth = `GET) ?(body = "") ?(sign = None) url =
  let open Cohttp in
  let open Cohttp_lwt_unix in
  let uri = Uri.of_string url in

  (* NOTE: Ad-hoc scheme rewriting (https -> http) for localhost
     for better dev experience *)
  let uri =
    match Uri.scheme uri with
    | Some "https"
      when [ Some "localhost"; Some "127.0.0.1" ] |> List.mem (Uri.host uri) ->
        Uri.with_scheme uri (Some "http")
    | _ -> uri
  in

  let meth_s = Method.to_string meth in
  let headers =
    let headers =
      ("content-length", body |> String.length |> string_of_int)
      :: ("connection", "close")
      :: ("host", Uri.http_host uri)
      :: ("date", Ptime.(now () |> to_http_date))
      :: headers
    in
    let headers =
      match sign with
      | None -> headers
      | Some (priv_key, key_id, signed_headers) ->
          Signature.sign ~priv_key ~key_id ~signed_headers ~headers ~meth
            ~path:(Uri.path_query_fragment uri)
            ~body:(Some body)
    in
    Header.of_list headers
  in
  try%lwt
    let%lwt resp, body =
      match meth with
      | `GET -> Client.get ~headers uri
      | `POST ->
          let body = Cohttp_lwt.Body.of_string body in
          Client.post ~headers ~body uri
      | _ -> failwith "Not implemented method"
    in
    let status = Response.status resp in
    Log.debug (fun m ->
        m "[fetch] %s %s --> %s" meth_s url (Code.string_of_status status));
    let headers =
      Response.headers resp |> Header.to_list
      |> List.map (fun (k, v) -> (String.lowercase_ascii k, v))
    in
    let%lwt body = Cohttp_lwt.Body.to_string body in
    Lwt.return_ok (status, headers, body)
  with e ->
    let backtrace = Printexc.get_backtrace () in
    Log.err (fun m ->
        m "[fetch] %s %s: %s\n%s" meth_s url (Printexc.to_string e) backtrace);
    Lwt.return_error ()

let fetch_exn ?(headers = []) ?(meth = `GET) ?(body = "") ?(sign = None)
    (url : string) : string Lwt.t =
  match%lwt fetch ~headers ~meth ~body ~sign url with
  | Ok (`OK, _, body) -> Lwt.return body
  | _ -> failwith "fetch_exn failed"
