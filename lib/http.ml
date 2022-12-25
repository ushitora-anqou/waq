open Httpaf
open Httpaf_lwt_unix

type request = {
  query : (string * string list) list;
  param : (string * string) list;
  body : string Lwt.t;
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
  let { Request.meth; target; _ } = Reqd.request reqd in
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
  Lwt.catch
    (fun () ->
      let open Lwt.Syntax in
      (* Invoke the handler *)
      let* res =
        try handler (make_request ~query ~param ~body ())
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
      Reqd.respond_with_string reqd
        (Response.create ~headers res.status)
        res.body;
      Log.info (fun m ->
          m "%s %s %s"
            (Status.to_string res.status)
            (Method.to_string meth) target);
      Lwt.return_unit)
    (fun e ->
      Log.err (fun m ->
          m "Unexpected exception: %s %s: %s\n%s" (Method.to_string meth) target
            (Printexc.to_string e)
            (Printexc.get_backtrace ()));
      Lwt.return_unit)

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
      let open Lwt.Syntax in
      let* _ = Lwt_io.establish_server_with_client_socket addr handler in
      f ();
      Lwt.return_unit);
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

let param (k : string) (r : request) : string = List.assoc k r.param
let body (r : request) : string Lwt.t = r.body

let fetch ?(headers = []) ?(meth = `GET) ?(body = "") url : string Lwt.t =
  let open Lwt.Syntax in
  let url = Uri.of_string url in
  let host = Uri.host url |> Option.get in
  let scheme = Uri.scheme url |> Option.get in
  let port = url |> Uri.port |> Option.fold ~none:scheme ~some:string_of_int in
  let path = url |> Uri.path in
  let* addr = Lwt_unix.getaddrinfo host port [ Unix.(AI_FAMILY PF_INET) ] in
  let socket = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  let* () = Lwt_unix.connect socket (List.hd addr).Unix.ai_addr in
  let promise, resolver = Lwt.wait () in
  let headers =
    ("content-length", body |> String.length |> string_of_int)
    :: ("connection", "close") :: ("host", host) :: headers
    |> Headers.of_list
  in
  let response_handler response response_body =
    match response with
    | { Response.status = `OK; _ } ->
        Lwt.async @@ fun () ->
        let* body = read_body_async response_body in
        Lwt.wakeup_later resolver body;
        Lwt.return_unit
    | response ->
        Format.fprintf Format.err_formatter "%a\n%!" Response.pp_hum response;
        exit 1
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
  promise
