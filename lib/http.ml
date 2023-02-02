open Util
open Lwt.Infix

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

module Header = struct
  type name =
    [ `Accept
    | `Accept_encoding
    | `Access_control_allow_headers
    | `Access_control_allow_methods
    | `Access_control_allow_origin
    | `Access_control_request_headers
    | `Authorization
    | `Connection
    | `Content_length
    | `Content_type
    | `Date
    | `Digest
    | `Host
    | `Location
    | `Raw of string
    | `Sec_websocket_protocol
    | `Signature
    | `User_agent ]

  let lower_string_of_name : name -> string = function
    | `Accept -> "accept"
    | `Accept_encoding -> "accept-encoding"
    | `Access_control_allow_headers -> "access-control-allow-headers"
    | `Access_control_allow_methods -> "access-control-allow-methods"
    | `Access_control_allow_origin -> "access-control-allow-origin"
    | `Access_control_request_headers -> "access-control-request-headers"
    | `Authorization -> "authorization"
    | `Connection -> "connection"
    | `Content_length -> "content-length"
    | `Content_type -> "content-type"
    | `Date -> "date"
    | `Digest -> "digest"
    | `Host -> "host"
    | `Location -> "location"
    | `Raw s -> s
    | `Sec_websocket_protocol -> "sec-websocket-protocol"
    | `Signature -> "signature"
    | `User_agent -> "user-agent"

  let string_of_name = lower_string_of_name

  let name_of_string (k : string) : name =
    match String.lowercase_ascii k with
    | "accept" -> `Accept
    | "accept-encoding" -> `Accept_encoding
    | "access-control-allow-headers" -> `Access_control_allow_headers
    | "access-control-allow-methods" -> `Access_control_allow_methods
    | "access-control-allow-origin" -> `Access_control_allow_origin
    | "access-control-request-headers" -> `Access_control_request_headers
    | "authorization" -> `Authorization
    | "connection" -> `Connection
    | "content-length" -> `Content_length
    | "content-type" -> `Content_type
    | "date" -> `Date
    | "digest" -> `Digest
    | "host" -> `Host
    | "location" -> `Location
    | "sec-websocket-protocol" -> `Sec_websocket_protocol
    | "signature" -> `Signature
    | "user-agent" -> `User_agent
    | s -> `Raw s

  type t = name * string

  let to_tuple ((n, v) : t) : string * string = (string_of_name n, v)
  let of_tuple (n, v) : t = (name_of_string n, v)
end

module Headers = struct
  type t = Header.t list

  let to_list : t -> (string * string) list = List.map Header.to_tuple
  let of_list : (string * string) list -> t = List.map Header.of_tuple
end

module PathPattern = struct
  type single_pattern = L of string | P of string | S
  type t = single_pattern list

  let split_on_slash = String.split_on_char '/' |.> List.tl

  let of_string (src : string) : t =
    src |> split_on_slash
    |> List.map (function
         | "*" -> S
         | x when String.starts_with ~prefix:":" x -> P x
         | x -> L x)

  let perform ~(pat : t) (src : string) : (string * string) list option =
    let rec aux param = function
      | [], [] | _, [ S ] -> Some param
      | x :: xs, L y :: ys when x = y -> aux param (xs, ys)
      | x :: xs, P y :: ys -> aux ((y, x) :: param) (xs, ys)
      | _ -> None
    in
    aux [] (split_on_slash src, pat)
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
      ~(headers : Headers.t) ~(meth : Method.t) ~(path : string) : string =
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
           let digest = Sha256.(string body |> to_bin |> Base64.encode_exn) in
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
      make_signature_header ~key_id ~signature ~algorithm
        ~headers:signed_headers ()
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
      ~(signed_headers : string list) ~(signature : string)
      ~(headers : Headers.t) ~(meth : Method.t) ~(path : string)
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

module Client = struct
  let fetch ?(headers = []) ?(meth = `GET) ?(body = "") ?(sign = None) url =
    let open Cohttp in
    let open Cohttp_lwt_unix in
    let uri = Uri.of_string url in

    (* NOTE: Ad-hoc scheme rewriting (https -> http) for localhost
       for better dev experience *)
    let uri =
      match Uri.scheme uri with
      | Some "https"
        when [ Some "localhost"; Some "127.0.0.1" ] |> List.mem (Uri.host uri)
        ->
          Uri.with_scheme uri (Some "http")
      | _ -> uri
    in

    let meth_s = Method.to_string meth in
    let headers =
      let headers =
        (`Content_length, body |> String.length |> string_of_int)
        :: (`Connection, "close")
        :: (`Host, Uri.http_host uri)
        :: (`Date, Ptime.(now () |> to_http_date))
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
      headers |> Headers.to_list |> Header.of_list
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
end

module BareServer = struct
  module Request = struct
    include Cohttp_lwt.Request

    let headers = headers |.> Cohttp.Header.to_list
  end

  module Response = struct
    type t = Cohttp_lwt_unix.Server.response_action
  end

  module Body = Cohttp_lwt.Body

  let respond ~(status : Status.t) ~(headers : Headers.t) ~(body : string) =
    let headers = headers |> Headers.to_list |> Cohttp.Header.of_list in
    Cohttp_lwt_unix.Server.respond_string ~status ~headers ~body () >|= fun x ->
    `Response x

  let start_server port k callback =
    let callback _conn (req : Request.t) (body : Body.t) =
      (* Invoke the handler *)
      try%lwt callback req body
      with e ->
        let uri = Request.uri req in
        let meth = Request.meth req in
        Log.err (fun m ->
            m "Unexpected exception: %s %s: %s\n%s" (Method.to_string meth)
              (Uri.to_string uri) (Printexc.to_string e)
              (Printexc.get_backtrace ()));
        respond ~status:`Internal_server_error ~body:"" ~headers:[]
    in
    let server =
      let open Cohttp_lwt_unix in
      Server.make_response_action ~callback ()
      |> Server.create ~mode:(`TCP (`Port port))
    in
    Lwt.join [ server; k ] |> Lwt_main.run

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

  let websocket (req : Request.t) f =
    let recv_stream, recv_stream_push = Lwt_stream.create () in
    let conn = make_ws_conn ~recv_stream () in
    let%lwt resp, frames_out_fn =
      Websocket_cohttp_lwt.upgrade_connection req (fun { opcode; content; _ } ->
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
              |> Option.(some |.> get conn.frames_out_fn))
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
end

module Server = struct
  type request_body =
    | JSON of Yojson.Safe.t
    | Form of (string * string list) list

  type request =
    | Request of {
        bare_req : BareServer.Request.t;
        bare_body : BareServer.Body.t;
        meth : Method.t;
        uri : Uri.t;
        path : string;
        query : (string * string list) list;
        param : (string * string) list;
        body : request_body;
        raw_body : string;
        headers : Headers.t;
      }

  type response =
    | BareResponse of BareServer.Response.t
    | Response of { status : Status.t; headers : Headers.t; body : string }

  type handler = request -> response Lwt.t
  type middleware = handler -> handler

  exception ErrorResponse of { status : Status.t; body : string }

  let raise_error_response ?(body = "") status =
    raise (ErrorResponse { status; body })

  let respond ?(status = `OK) ?(headers = []) (body : string) =
    Response { status; headers; body } |> Lwt.return

  let body = function Request { raw_body; _ } -> raw_body
  let param name = function Request { param; _ } -> List.assoc name param

  let query ?default name = function
    | Request { body; query; _ } -> (
        try
          match body with
          | JSON (`Assoc l) -> (
              match List.assoc name l with
              | `Bool b -> string_of_bool b
              | `Int i -> string_of_int i
              | `String s -> s
              | _ -> failwith "json assoc")
          | JSON _ -> failwith "json"
          | Form body -> (
              match List.assoc_opt name query with
              | Some l -> List.hd l
              | None -> body |> List.assoc name |> List.hd)
        with
        | _ when default <> None -> Option.get default
        | _ -> raise_error_response `Bad_request)

  let query_opt name r = try Some (query name r) with _ -> None

  let header_opt name : request -> string option = function
    | Request { headers; _ } -> headers |> List.assoc_opt name

  let header name (r : request) : string =
    match header_opt name r with None -> failwith "header: none" | Some v -> v

  let start_server ?(port = 8080) (handler : handler) k : unit =
    BareServer.start_server port (k ())
    @@ fun (req : BareServer.Request.t) (body : BareServer.Body.t) :
      BareServer.Response.t Lwt.t ->
    let uri = BareServer.Request.uri req in
    let meth = BareServer.Request.meth req in
    let headers = BareServer.Request.headers req |> Headers.of_list in
    let path = Uri.path uri in
    let query = Uri.query uri in

    (* Parse body *)
    let%lwt raw_body = BareServer.Body.to_string body in
    let parsed_body =
      if raw_body = "" then Form []
      else
        match List.assoc_opt `Content_type headers with
        | Some "application/json" -> JSON (Yojson.Safe.from_string raw_body)
        | Some "application/x-www-form-urlencoded" | _ ->
            Form (raw_body |> Uri.query_of_encoded)
    in

    (* Construct request *)
    let req =
      Request
        {
          bare_req = req;
          bare_body = body;
          meth;
          uri;
          path;
          query;
          param = [];
          body = parsed_body;
          raw_body;
          headers;
        }
    in

    (* Invoke the handler *)
    match%lwt
      try%lwt handler req
      with ErrorResponse { status; body } -> respond ~status body
    with
    | BareResponse resp -> Lwt.return resp
    | Response { status; headers; body } ->
        BareServer.respond ~status ~headers ~body

  (* WebSocket *)
  type ws_conn = BareServer.ws_conn

  let ws_send = BareServer.ws_send
  let ws_recv = BareServer.ws_recv

  let websocket (r : request) f =
    match r with
    | Request { bare_req; _ } ->
        BareServer.websocket bare_req f >|= fun r -> BareResponse r

  (* Router *)
  type route = Method.t * PathPattern.t * (request -> response Lwt.t)

  let default_handler : handler = function
    | Request req ->
        let status =
          match req.meth with `GET -> `Not_found | _ -> `Method_not_allowed
        in
        respond ~status ""

  let router (routes : route list) (inner_handler : handler) (req : request) :
      response Lwt.t =
    match req with
    | Request req ->
        (* Choose correct handler from routes *)
        let param, handler =
          routes
          |> List.find_map (fun (meth', pat, handler) ->
                 if req.meth <> meth' then None
                 else
                   PathPattern.perform ~pat req.path
                   |> Option.map (fun param -> (param, handler)))
          |> Option.value ~default:([], inner_handler)
        in
        let req = Request { req with param } in
        handler req

  let get target f : route = (`GET, PathPattern.of_string target, f)
  let post target f : route = (`POST, PathPattern.of_string target, f)
  let options target f : route = (`OPTIONS, PathPattern.of_string target, f)

  (* Middleware CORS *)

  module Cors = struct
    type t = {
      target : PathPattern.t;
      methods : Method.t list;
      origin : string;
    }

    let make target ?(origin = "*") ~methods () =
      { target = PathPattern.of_string target; methods; origin }
  end

  let middleware_cors (src : Cors.t list) (inner_handler : handler)
      (req : request) : response Lwt.t =
    (* Handler for preflight OPTIONS requests *)
    let handler (r : Cors.t) (req : request) : response Lwt.t =
      let headers =
        [
          ( `Access_control_allow_methods,
            r.methods |> List.map Method.to_string |> String.concat ", " );
          (`Access_control_allow_origin, r.origin);
        ]
      in
      let headers =
        match req with
        | Request req ->
            req.headers
            |> List.assoc_opt `Access_control_request_headers
            |> Option.fold ~none:headers ~some:(fun v ->
                   (`Access_control_allow_headers, v) :: headers)
      in
      respond ~status:`No_content ~headers ""
    in

    (* Construct routes for preflight requests *)
    let routes =
      src |> List.map (fun (r : Cors.t) -> (`OPTIONS, r.target, handler r))
    in

    (* Construct router *)
    req
    |> router routes @@ function
       (* Fallback handler: apply inner_handler, and
          if path matches, append "access-control-allow-origin" header *)
       | Request { path; _ } as req -> (
           let path_match =
             src
             |> List.find_opt (fun Cors.{ target; _ } ->
                    PathPattern.perform ~pat:target path |> Option.is_some)
           in
           inner_handler req >|= fun resp ->
           match (resp, path_match) with
           | _, None | BareResponse _, _ -> resp
           | Response res, Some { origin; _ } ->
               Response
                 {
                   res with
                   headers =
                     (`Access_control_allow_origin, origin) :: res.headers;
                 })

  (* Middlware Logger *)
  let middleware_logger (inner_handler : handler) (req : request) :
      response Lwt.t =
    let (Request { uri; meth; _ }) = req in
    let meth = Method.to_string meth in
    let uri = Uri.to_string uri in
    Log.debug (fun m -> m "%s %s" meth uri);
    inner_handler req >|= fun resp ->
    (match resp with
    | Response { status; _ } ->
        Log.info (fun m -> m "%s %s %s" (Status.to_string status) meth uri)
    | BareResponse _ -> Log.info (fun m -> m "[bare] %s %s" meth uri));
    resp
end
