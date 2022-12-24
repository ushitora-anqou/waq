open Httpaf
open Httpaf_lwt_unix

type request = {
  query : (string * string list) list;
  param : (string * string) list;
}
[@@deriving make]

type response = {
  status : Status.t;
  headers : (string * string) list;
  body : string; [@default ""]
}
[@@deriving make]

type handler = request -> response Lwt.t
type path = [ `L of string ] list
type method_ = Method.t
type route = method_ * path * handler

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
  let handler =
    let default_handler =
      Fun.const @@ Lwt.return
      @@ make_response
           ~status:
             (match meth with `GET -> `Not_found | _ -> `Method_not_allowed)
           ()
    in
    let rec match_path = function
      | [], [] -> Some ()
      | x :: xs, `L y :: ys when x = y -> match_path (xs, ys)
      | _ -> None
    in
    routes
    |> List.find_map (fun (meth', ptn, handler) ->
           if meth <> meth' then None
           else match_path (path, ptn) |> Option.map (Fun.const handler))
    |> Option.value ~default:default_handler
  in
  (* Return the response asynchronously *)
  Lwt.async @@ fun () ->
  Lwt.catch
    (fun () ->
      let open Lwt.Syntax in
      (* Invoke the handler *)
      let* res = handler (make_request ~query ()) in
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
          m "Exception caught: %s %s: %s" (Method.to_string meth) target
            (Printexc.to_string e));
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
  src |> String.split_on_char '/' |> List.tl |> List.map (fun x -> `L x)

let get (path : string) (handler : handler) =
  let path = parse_path path in
  (`GET, path, handler)

let respond ?(status = `OK) ?(headers = []) (body : string) : response Lwt.t =
  Lwt.return @@ make_response ~status ~headers ~body ()
