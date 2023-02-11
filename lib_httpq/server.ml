open Util
open Lwt.Infix

type request_body =
  | JSON of Yojson.Safe.t
  | Form of (string * string list) list

type request =
  | Request of {
      bare_req : Bare_server.Request.t;
      bare_body : Bare_server.Body.t;
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
  | BareResponse of Bare_server.Response.t
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

let query_many_opt name : request -> string list option = function
  | Request { query; _ } ->
      (* FIXME: parse body *)
      List.assoc_opt name query

let query_many ?default name req =
  match query_many_opt name req with
  | Some r -> r
  | None when default <> None -> Option.get default
  | None -> raise_error_response `Bad_request

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

let default_handler : handler = function
  | Request req ->
      let status =
        match req.meth with `GET -> `Not_found | _ -> `Method_not_allowed
      in
      respond ~status ""

let start_server ?(port = 8080) (handler : handler) k : unit =
  Bare_server.start_server port (k ())
  @@ fun (req : Bare_server.Request.t) (body : Bare_server.Body.t) :
    Bare_server.Response.t Lwt.t ->
  let uri = Bare_server.Request.uri req in
  let meth = Bare_server.Request.meth req in
  let headers = Bare_server.Request.headers req |> Headers.of_list in
  let path = Uri.path uri in
  let query = Uri.query uri in

  (* Parse body *)
  let%lwt raw_body = Bare_server.Body.to_string body in
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
      Bare_server.respond ~status ~headers ~body

(* WebSocket *)
type ws_conn = Bare_server.ws_conn

let ws_send = Bare_server.ws_send
let ws_recv = Bare_server.ws_recv

let websocket (r : request) f =
  match r with
  | Request { bare_req; _ } ->
      Bare_server.websocket bare_req f >|= fun r -> BareResponse r

(* Middleware Router *)
module Router = struct
  type route = Method.t * string * (request -> response Lwt.t)

  type spec_entry = Route of route | Scope of (string * spec)
  and spec = spec_entry list

  let use (spec : spec) (inner_handler : handler) (req : request) :
      response Lwt.t =
    let routes =
      let rec aux (spec : spec) : route list =
        spec
        |> List.map (function
             | Route r -> [ r ]
             | Scope (name, spec) ->
                 aux spec
                 |> List.map (fun (meth, uri, h) -> (meth, name ^ uri, h)))
        |> List.flatten
      in
      aux spec
      |> List.map (fun (meth, uri, h) -> (meth, Path_pattern.of_string uri, h))
    in

    match req with
    | Request req ->
        (* Choose correct handler from routes *)
        let param, handler =
          routes
          |> List.find_map (fun (meth', pat, handler) ->
                 if req.meth <> meth' then None
                 else
                   Path_pattern.perform ~pat req.path
                   |> Option.map (fun param -> (param, handler)))
          |> Option.value ~default:([], inner_handler)
        in
        let req = Request { req with param } in
        handler req

  let get target f : spec_entry = Route (`GET, target, f)
  let post target f : spec_entry = Route (`POST, target, f)
  let options target f : spec_entry = Route (`OPTIONS, target, f)
  let scope (name : string) (spec : spec) : spec_entry = Scope (name, spec)
end

(* Middleware CORS *)
module Cors = struct
  type t = {
    target : string;
    target_pat : Path_pattern.t;
    methods : Method.t list;
    origin : string;
  }

  let make target ?(origin = "*") ~methods () =
    { target; target_pat = Path_pattern.of_string target; methods; origin }

  let use (src : t list) (inner_handler : handler) (req : request) :
      response Lwt.t =
    (* Handler for preflight OPTIONS requests *)
    let handler (r : t) (req : request) : response Lwt.t =
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
      src |> List.map (fun (r : t) -> Router.options r.target (handler r))
    in

    (* Construct router *)
    req
    |> Router.use routes @@ function
       (* Fallback handler: apply inner_handler, and
          if path matches, append "access-control-allow-origin" header *)
       | Request { path; _ } as req -> (
           let path_match =
             src
             |> List.find_opt (fun { target_pat; _ } ->
                    Path_pattern.perform ~pat:target_pat path |> Option.is_some)
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
end

(* Middlware Logger *)
module Logger = struct
  let use (inner_handler : handler) (req : request) : response Lwt.t =
    let (Request { uri; meth; _ }) = req in
    let meth = Method.to_string meth in
    let uri = Uri.to_string uri in
    Logq.debug (fun m -> m "%s %s" meth uri);
    inner_handler req >|= fun resp ->
    (match resp with
    | Response { status; _ } ->
        Logq.info (fun m -> m "%s %s %s" (Status.to_string status) meth uri)
    | BareResponse _ -> Logq.info (fun m -> m "[bare] %s %s" meth uri));
    resp
end
