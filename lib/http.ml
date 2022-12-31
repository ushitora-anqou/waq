open Httpaf
open Httpaf_lwt_unix

module Signature = struct
  type private_key = X509.Private_key.t
  type public_key = X509.Public_key.t
  type keypair = private_key * public_key

  let encode_public_key (pub_key : public_key) : string =
    X509.Public_key.encode_pem pub_key |> Cstruct.to_string

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

  let initialize () = Mirage_crypto_rng_lwt.initialize ()

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
  query : (string * string list) list;
  param : (string * string) list;
  body : string Lwt.t;
  headers : (string * string) list;
}
[@@deriving make]

type response = {
  status : Status.t;
  headers : (string * string) list;
  body : string; [@default ""]
}
[@@deriving make]

type handler = request -> response Lwt.t
type path = [ `L of string | `P of string ] list
type method_ = Method.t
type route = method_ * path * handler

let read_body_async (body : [ `read ] Body.t) : string Lwt.t =
  let promise, resolver = Lwt.wait () in
  let length = ref 0 in
  let buffer = ref (Bigstringaf.create 4096) in
  (* Callbacks *)
  let rec on_read chunk ~off ~len =
    let new_length = !length + len in
    if new_length > Bigstringaf.length !buffer then (
      (* Resize buffer *)
      let new_buffer = Bigstringaf.create (new_length * 2) in
      Bigstringaf.blit !buffer ~src_off:0 new_buffer ~dst_off:0 ~len:!length;
      buffer := new_buffer);
    (* Copy the chunk to the buffer *)
    Bigstringaf.blit chunk ~src_off:off !buffer ~dst_off:!length ~len;
    length := new_length;
    Body.schedule_read body ~on_eof ~on_read
  and on_eof () =
    Bigstringaf.sub !buffer ~off:0 ~len:!length
    |> Bigstringaf.to_string |> Lwt.wakeup_later resolver
  in
  Body.schedule_read body ~on_eof ~on_read;
  promise

let request_handler (routes : route list) (_ : Unix.sockaddr) (reqd : Reqd.t) :
    unit =
  let { Request.meth; target; headers; _ } = Reqd.request reqd in
  let headers = headers |> Headers.to_list in
  Log.debug (fun m -> m "%s %s" (Method.to_string meth) target);

  (* Parse target *)
  let path, query =
    let u = Uri.of_string target in
    (Uri.path u |> String.split_on_char '/' |> List.tl, Uri.query u)
  in

  (* Choose correct handler via router *)
  let param, handler =
    let default_handler =
      Fun.const @@ Lwt.return
      @@ make_response
           ~status:
             (match meth with `GET -> `Not_found | _ -> `Method_not_allowed)
           ()
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

  (* Start a thread to read the body *)
  let body = read_body_async (Reqd.request_body reqd) in

  (* Return the response asynchronously *)
  Lwt.async @@ fun () ->
  try%lwt
    (* Invoke the handler *)
    let%lwt res =
      try%lwt handler (make_request ~query ~param ~body ~headers ())
      with e ->
        Log.err (fun m ->
            m "Exception: %s %s: %s\n%s" (Method.to_string meth) target
              (Printexc.to_string e)
              (Printexc.get_backtrace ()));
        Lwt.return @@ make_response ~status:`Internal_server_error ()
    in
    (* Construct headers *)
    let headers =
      let src =
        ("Content-length", res.body |> String.length |> string_of_int)
        (* :: ("Connection", "close") *)
        :: res.headers
      in
      let t = Hashtbl.create (List.length src) in
      src |> List.iter (fun (k, v) -> Hashtbl.replace t k v);
      t |> Hashtbl.to_seq |> List.of_seq |> Headers.of_list
    in
    (* Format the response *)
    Reqd.respond_with_string reqd (Response.create ~headers res.status) res.body;
    Log.info (fun m ->
        m "%s %s %s"
          (Status.to_string res.status)
          (Method.to_string meth) target);
    Lwt.return_unit
  with e ->
    Log.err (fun m ->
        m "Unexpected exception: %s %s: %s\n%s" (Method.to_string meth) target
          (Printexc.to_string e)
          (Printexc.get_backtrace ()));
    Lwt.return_unit

(* FIXME: What is this function for? *)
let error_handler (_ : Unix.sockaddr) ?request:_
    (error : Server_connection.error) start_response =
  let response_body = start_response Headers.empty in
  (match error with
  | `Exn exn ->
      Body.write_string response_body (Printexc.to_string exn);
      Body.write_string response_body "\n"
  | (`Bad_request | `Bad_gateway | `Internal_server_error) as error ->
      Body.write_string response_body (Status.default_reason_phrase error));
  Body.close_writer response_body

let start_server ?(host = "127.0.0.1") ?(port = 8080) f routes =
  let host =
    let open Unix in
    try inet_addr_of_string host
    with Failure _ -> (gethostbyname host).h_addr_list.(0)
  in
  let addr = Unix.(ADDR_INET (host, port)) in
  let handler =
    Server.create_connection_handler ~error_handler
      ~request_handler:(request_handler routes)
  in
  Lwt.async (fun () ->
      let%lwt _ = Lwt_io.establish_server_with_client_socket addr handler in
      f ());
  let forever, _ = Lwt.wait () in
  Lwt_main.run forever

let router (routes : route list) = routes

let parse_path (src : string) : path =
  src |> String.split_on_char '/' |> List.tl
  |> List.map (function
       | x when String.starts_with ~prefix:":" x -> `P x
       | x -> `L x)

let get (path : string) (handler : handler) =
  let path = parse_path path in
  (`GET, path, handler)

let post (path : string) (handler : handler) =
  let path = parse_path path in
  (`POST, path, handler)

let respond ?(status = `OK) ?(headers = []) (body : string) : response Lwt.t =
  Lwt.return @@ make_response ~status ~headers ~body ()

let query_opt (k : string) (r : request) : string list option =
  List.assoc_opt k r.query

let query (k : string) (r : request) : string list = query_opt k r |> Option.get
let param (k : string) (r : request) : string = List.assoc k r.param
let body (r : request) : string Lwt.t = r.body
let headers (r : request) : (string * string) list = r.headers

let fetch ?(headers = []) ?(meth = `GET) ?(body = "") ?(sign = None)
    (url : string) : (Status.t * string, unit) result Lwt.t =
  try%lwt
    let url = Uri.of_string url in
    let host = Uri.host url |> Option.get in
    let scheme = Uri.scheme url |> Option.get in
    let port =
      url |> Uri.port |> Option.fold ~none:scheme ~some:string_of_int
    in
    let path = url |> Uri.path in
    let%lwt addr =
      Lwt_unix.getaddrinfo host port [ Unix.(AI_FAMILY PF_INET) ]
    in
    let socket = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    let%lwt () = Lwt_unix.connect socket (List.hd addr).Unix.ai_addr in
    let promise, resolver = Lwt.wait () in
    let headers =
      let headers =
        ("content-length", body |> String.length |> string_of_int)
        :: ("connection", "close") :: ("host", host)
        :: ("date", Time.(now () |> to_http_date))
        :: headers
      in
      let headers =
        match sign with
        | None -> headers
        | Some (priv_key, key_id, signed_headers) ->
            Signature.sign ~priv_key ~key_id ~signed_headers ~headers ~meth
              ~path ~body:(Some body)
      in
      Headers.of_list headers
    in
    let response_handler response response_body =
      match response with
      | { Response.status; _ } ->
          Lwt.async @@ fun () ->
          let%lwt body = read_body_async response_body in
          Lwt.wakeup_later resolver (status, body);
          Lwt.return_unit
    in
    let error_handler error =
      let error =
        match error with
        | `Malformed_response err -> Format.sprintf "Malformed response: %s" err
        | `Invalid_response_body_length _ -> "Invalid body length"
        | `Exn exn -> Format.sprintf "Exn raised: %s" (Printexc.to_string exn)
      in
      Log.err (fun m -> m "Error handling response: %s" error)
    in
    let request_body =
      Client.request ~response_handler ~error_handler socket
        (Request.create ~headers meth path)
    in
    Body.write_string request_body body;
    Body.close_writer request_body;
    let%lwt status, body = promise in
    Log.debug (fun m ->
        m "[fetch] %s %s --> %s \"%s\"" (Method.to_string meth)
          (Uri.to_string url) (Status.to_string status) body);
    Lwt.return (Ok (status, body))
  with e ->
    let backtrace = Printexc.get_backtrace () in
    Log.err (fun m ->
        m "[fetch] %s %s: %s\n%s" (Method.to_string meth) url
          (Printexc.to_string e) backtrace);
    Lwt.return (Error ())
