type formdata_t = {
  filename : string option;
  content_type : Multipart_form.Content_type.t;
  content : string;
}
[@@deriving make]

type request_body =
  | JSON of Yojson.Safe.t
  | Form of (string * string list) list
  | MultipartFormdata of { loaded : (string * formdata_t) list }

type request =
  | Request of {
      bare_req : Bare_server.Request.t;
      bare_body : Bare_server.Body.t;
      meth : Method.t;
      uri : Uri.t;
      path : string;
      query : (string * string) list;
      param : (string * string) list;
      body : (string option * request_body option) Lazy.t;
      headers : Headers.t;
    }

type response =
  | BareResponse of Bare_server.Response.t
  | Response of {
      status : Status.t;
      headers : Headers.t;
      body : string;
      tags : string list;
    }

type handler = Eio_unix.Stdenv.base -> request -> response
type middleware = handler -> handler

exception ErrorResponse of { status : Status.t; body : string }

let raise_error_response ?(body = "") status =
  raise (ErrorResponse { status; body })

let respond ?(status = `OK) ?(headers = []) ?(tags = []) (body : string) =
  Response { status; headers; body; tags }

let body = function
  | Request { body; _ } -> (
      match Lazy.force body with
      | Some raw_body, _ -> raw_body
      | _ -> failwith "body: none")

let param name = function Request { param; _ } -> List.assoc name param

let string_of_yojson_atom = function
  | `Bool b -> string_of_bool b
  | `Int i -> string_of_int i
  | `String s -> s
  | _ -> failwith "string_of_yojson_atom"

let list_assoc_many k1 l =
  l |> List.filter_map (fun (k, v) -> if k = k1 then Some v else None)

let query_many name : request -> string list = function
  | Request { body; query; _ } -> (
      match query |> list_assoc_many (name ^ "[]") with
      | _ :: _ as res -> res
      | [] -> (
          match Lazy.force body with
          | _, Some (JSON (`Assoc l)) -> (
              match List.assoc_opt name l with
              | Some (`List l) -> l |> List.map string_of_yojson_atom
              | _ -> [])
          | _ -> failwith "query many"))

let formdata name = function
  | Request { body; _ } -> (
      match Lazy.force body with
      | _, Some (MultipartFormdata { loaded; _ }) -> (
          match List.assoc_opt name loaded with
          | Some s -> Ok s
          | None -> Error ("The name " ^ name ^ " was not found"))
      | _ -> Error "formdata: not MultipartFormdata"
      | exception _ -> Error "Couldn't read body")

let formdata_exn name r =
  match formdata name r with
  | Ok s -> s
  | Error _ -> raise_error_response `Bad_request

let query ?default name req =
  match req with
  | Request { body; query; _ } -> (
      try
        match Lazy.force body |> snd with
        | None -> failwith "query: body none"
        | Some x -> (
            match x with
            | JSON (`Assoc l) -> List.assoc name l |> string_of_yojson_atom
            | JSON _ -> failwith "json"
            | Form body -> (
                match query |> List.assoc_opt name with
                | Some x -> x
                | None -> body |> List.assoc name |> List.hd)
            | MultipartFormdata _ ->
                let f = formdata_exn name req in
                f.content)
      with
      | _ when default <> None -> Option.get default
      | _ -> raise_error_response `Bad_request)

let query_opt name r = try Some (query name r) with _ -> None

let header_opt name : request -> string option = function
  | Request { headers; _ } -> headers |> List.assoc_opt name

let header name (r : request) : string =
  match header_opt name r with None -> failwith "header: none" | Some v -> v

let headers = function Request { headers; _ } -> headers
let path = function Request { path; _ } -> path
let meth = function Request { meth; _ } -> meth

let load_formdata_from_stream out_stream =
  let open Multipart_form in
  let rec save_part loaded raw =
    match Eio.Stream.take_nonblocking out_stream with
    | None -> loaded
    | Some (_, hdr, contents) ->
        let loaded =
          let ( let* ) v f = v |> Option.fold ~none:loaded ~some:f in
          let* cd = Header.content_disposition hdr in
          let* name = Content_disposition.name cd in
          let filename = Content_disposition.filename cd in
          let buf = Buffer.create 0 in
          let rec loop () =
            match Eio.Stream.take_nonblocking contents with
            | None -> ()
            | Some s ->
                Buffer.add_string buf s;
                loop ()
          in
          loop ();
          ( name,
            make_formdata_t ?filename ~content_type:(Header.content_type hdr)
              ~content:(Buffer.contents buf) () )
          :: loaded
        in
        save_part loaded raw
  in
  save_part [] []

let parse_body ~body ~headers =
  let content_type = List.assoc_opt `Content_type headers in
  match
    content_type
    |> Option.map (fun s ->
           s |> String.split_on_char ';' |> List.hd |> String.trim)
  with
  | Some "multipart/form-data" ->
      (* FIXME: Currenty all contents are stored on memory.
         This is not the best choice from perspective of space efficiency.
         We should utilize files on disk *)
      let load_body () =
        let open Multipart_form in
        let content_type =
          match Content_type.of_string (Option.get content_type ^ "\r\n") with
          | Ok s -> s
          | Error (`Msg _msg) -> raise_error_response `Bad_request
        in

        let in_stream = Eio.Stream.create 1 in
        Eio.Stream.add in_stream (Bare_server.Body.to_string body);
        Eio.Switch.run @@ fun sw ->
        let th, out_stream =
          Multipart_form_eio.stream ~sw ~identify:Fun.id in_stream content_type
        in
        match Eio.Promise.await_exn th with
        | _ -> load_formdata_from_stream out_stream
        | exception _ -> raise_error_response `Bad_request
      in
      (None, MultipartFormdata { loaded = load_body () })
  | Some "application/json" -> (
      let raw_body = Bare_server.Body.to_string body in
      ( Some raw_body,
        try JSON (Yojson.Safe.from_string raw_body) with _ -> Form [] ))
  | Some "application/x-www-form-urlencoded" | _ ->
      let raw_body = Bare_server.Body.to_string body in
      (Some raw_body, Form (Uri.query_of_encoded raw_body))

let default_handler : handler =
 fun _env -> function
  | Request req ->
      let status =
        match req.meth with `GET -> `Not_found | _ -> `Method_not_allowed
      in
      respond ~status ""

type ws_conn = Bare_server.ws_conn

module Ws_conn_man = struct
  type t = {
    chan : (request * (ws_conn -> unit) * response Eio.Stream.t) Eio.Stream.t;
  }

  let global_runner = { chan = Eio.Stream.create 0 }

  let start_global_runner env ~sw =
    Eio.Fiber.fork ~sw (fun () ->
        let rec loop () =
          let req, callback, recv_stream = Eio.Stream.take global_runner.chan in
          let resp =
            match req with
            | Request { bare_req; _ } ->
                let r = Bare_server.websocket env ~sw bare_req callback in
                BareResponse r
          in
          Eio.Stream.add recv_stream resp;
          loop ()
        in
        loop ())

  let start_ws_conn req callback =
    let recv_chan = Eio.Stream.create 0 in
    Eio.Stream.add global_runner.chan (req, callback, recv_chan);
    Eio.Stream.take recv_chan
end

let websocket (r : request) f = Ws_conn_man.start_ws_conn r f
let ws_send = Bare_server.ws_send
let ws_recv = Bare_server.ws_recv

let start_server env ~sw ?(listen = `Tcp (Eio.Net.Ipaddr.V4.loopback, 8080))
    ?error_handler (handler : handler) k : unit =
  Ws_conn_man.start_global_runner env ~sw;
  Bare_server.start_server ~listen env ~sw k
  @@ fun (req : Bare_server.Request.t) (body : Bare_server.Body.t) :
    Bare_server.Response.t ->
  (* Parse req *)
  let uri = Bare_server.Request.uri req in
  let meth = Bare_server.Request.meth req in
  let headers = Bare_server.Request.headers req |> Headers.of_list in
  let path = Uri.path uri in
  let query =
    Uri.query uri |> List.map (fun (k, xs) -> (k, xs |> String.concat ","))
  in
  let lazy_parsed_body =
    lazy
      (match parse_body ~body ~headers with
      | exception _ -> (None, None)
      | raw_body, parsed_body -> (raw_body, Some parsed_body))
  in
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
        body = lazy_parsed_body;
        headers;
      }
  in

  (* Invoke the handler *)
  let res = handler env req in

  (* Respond (after call error_handler if necessary *)
  let rec aux first = function
    | BareResponse resp -> resp
    | Response { status; headers; body; _ }
      when (not first)
           || Option.is_none error_handler
           || not (Status.is_error status)
           (* - error_handler is already called;
              - error_handler is not specified; or
              - not erroneous response *) ->
        Bare_server.respond ~status ~headers ~body
    | Response { status; headers; body; _ } ->
        let error_handler = Option.get error_handler in
        error_handler ~req ~status ~headers ~body |> aux false
  in
  aux true res

(* Middleware Router *)
module Router = struct
  type route = Method.t * string * handler

  type spec_entry = Route of route | Scope of (string * spec)
  and spec = spec_entry list

  let use (spec : spec) (inner_handler : handler) env (req : request) : response
      =
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
    | Request req -> (
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
        let resp =
          try handler env req with
          | ErrorResponse { status; body } ->
              Logs.debug (fun m ->
                  m "Error response raised: %s\n%s" (Status.to_string status)
                    (Printexc.get_backtrace ()));
              respond ~status ~tags:[ "log" ] body
          | e ->
              Logs.debug (fun m ->
                  m "Exception raised: %s\n%s" (Printexc.to_string e)
                    (Printexc.get_backtrace ()));
              respond ~status:`Internal_server_error ""
        in
        match resp with
        | Response ({ status; tags; _ } as r) when Status.is_error status ->
            Response { r with tags = "log" :: tags }
        | r -> r)

  let get target f : spec_entry = Route (`GET, target, f)
  let post target f : spec_entry = Route (`POST, target, f)
  let patch target f : spec_entry = Route (`PATCH, target, f)
  let delete target f : spec_entry = Route (`DELETE, target, f)
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
    expose : Header.name list;
  }

  let make target ?(origin = "*") ~methods ?(expose = []) () =
    let target_pat = Path_pattern.of_string target in
    { target; target_pat; methods; origin; expose }

  let use (src : t list) (inner_handler : handler) env (req : request) :
      response =
    (* Handler for preflight OPTIONS requests *)
    let handler (r : t) _ (req : request) : response =
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
    Router.use routes
      (fun env -> function
        (* Fallback handler: apply inner_handler, and
           if path matches, append CORS headers *)
        | Request { path; _ } as req -> (
            let path_match =
              src
              |> List.find_opt (fun { target_pat; _ } ->
                     Path_pattern.perform ~pat:target_pat path |> Option.is_some)
            in
            let resp = inner_handler env req in
            match (resp, path_match) with
            | _, None | BareResponse _, _ -> resp
            | Response res, Some { origin; expose; _ } ->
                Response
                  {
                    res with
                    headers =
                      (`Access_control_allow_origin, origin)
                      :: ( `Access_control_expose_headers,
                           expose
                           |> List.map Header.string_of_name
                           |> String.concat ", " )
                      :: res.headers;
                  }))
      env req
end

(* Middlware Logger *)
module Logger = struct
  let string_of_request_response req res =
    match (req, res) with
    | Request { bare_req; body; _ }, Response { status; _ } ->
        let open Buffer in
        let buf = create 0 in
        let fmt = Format.formatter_of_buffer buf in
        Bare_server.Request.pp_hum fmt bare_req;
        Format.pp_print_flush fmt ();
        add_string buf "\n";
        let raw_body =
          if Http.Request.has_body bare_req = `Yes then
            body |> Lazy.force |> fst
          else None
        in
        raw_body
        |> Option.iter (fun s ->
               add_string buf "\n";
               add_string buf s;
               add_string buf "\n");
        add_string buf "\n==============================\n";
        add_string buf ("Status: " ^ Status.to_string status);
        Buffer.contents buf
    | _ -> assert false

  let now () = Unix.gettimeofday () |> Ptime.of_float_s |> Option.get

  let use ?dump_req_dir (inner_handler : handler) env (req : request) : response
      =
    let (Request { uri; meth; _ }) = req in
    let meth = Method.to_string meth in
    let uri = Uri.to_string uri in
    Logs.debug (fun m -> m "%s %s" meth uri);

    let resp = inner_handler env req in

    (match resp with
    | Response { status; _ } ->
        Logs.info (fun m -> m "%s %s %s" (Status.to_string status) meth uri)
    | BareResponse _ -> Logs.info (fun m -> m "[bare] %s %s" meth uri));

    (match resp with
    | Response { tags; _ } when List.mem "log" tags -> (
        match dump_req_dir with
        | None ->
            let s = string_of_request_response req resp in
            Logs.info (fun m -> m "Detail of request and response:\n%s" s)
        | Some _dir -> (* FIXME: implement here *) ())
    | _ -> ());

    resp
end
