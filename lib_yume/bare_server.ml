module Request = struct
  include Cohttp.Request

  let headers x = x |> headers |> Cohttp.Header.to_list
end

module Response = struct
  type t = Cohttp_eio.Server.response_action
end

module Body = struct
  include Cohttp_eio.Body

  let to_string body = Eio.Buf_read.(parse_exn take_all) body ~max_size:max_int
end

let respond ~(status : Status.t) ~(headers : Headers.t) ~(body : string) =
  let headers = headers |> Headers.to_list |> Cohttp.Header.of_list in
  `Response
    (Http.Response.make ~status ~headers (), Cohttp_eio.Body.of_string body)

let start_server ~listen ~sw env k callback =
  let callback _conn (req : Request.t) (body : Body.t) =
    (* Invoke the handler *)
    try callback req body
    with e ->
      let uri = Request.uri req in
      let meth = Request.meth req in
      Logs.err (fun m ->
          m "Unexpected exception: %s %s: %s\n%s" (Method.to_string meth)
            (Uri.to_string uri) (Printexc.to_string e)
            (Printexc.get_backtrace ()));
      respond ~status:`Internal_server_error ~body:"" ~headers:[]
  in
  let server = Cohttp_eio.Server.make_response_action ~callback () in
  let socket =
    Eio.Net.listen (Eio.Stdenv.net env) ~sw ~backlog:128 ~reuse_addr:true listen
  in
  Eio.Fiber.both
    (fun () -> k socket)
    (fun () -> Cohttp_eio.Server.run ~on_error:raise socket server)

type ws_conn = {
  mutable frames_out_fn : (Websocket.Frame.t option -> unit) option;
  mutable closed : bool; [@default false]
  recv_stream : string option Eio.Stream.t;
}
[@@deriving make]

let websocket_handler conn frames_out_fn frame =
  match frame with
  | None ->
      if not conn.closed then (
        conn.closed <- true;
        Eio.Stream.add conn.recv_stream None)
  | Some Websocket.Frame.{ opcode; content; _ } -> (
      match opcode with
      | Close ->
          Logs.debug (fun m -> m "Websocket: recv Close");
          frames_out_fn
            (Some (Websocket.Frame.create ~opcode:Close ~content:"" ()));
          conn.closed <- true;
          Eio.Stream.add conn.recv_stream None
      | Text | Binary ->
          Logs.debug (fun m -> m "Websocket: recv: %s" content);
          Eio.Stream.add conn.recv_stream (Some content)
      | Ping ->
          frames_out_fn (Some (Websocket.Frame.create ~opcode:Pong ~content ()))
      | Pong -> () (* Just ignore *)
      | _ -> frames_out_fn (Some (Websocket.Frame.close 1002)))

let ws_send ?(opcode = Websocket.Frame.Opcode.Text) (c : ws_conn) content =
  if not c.closed then
    Websocket.Frame.create ~opcode ~content ()
    |> Option.some |> Option.get c.frames_out_fn

let ws_recv (c : ws_conn) = Eio.Stream.take c.recv_stream

let websocket env ~sw (req : Request.t) f =
  let conn = make_ws_conn ~recv_stream:(Eio.Stream.create 10) () in
  let resp, frames_out_fn =
    Ws.Server.upgrade_connection req (websocket_handler conn)
  in
  conn.frames_out_fn <- Some frames_out_fn;
  Eio.Fiber.fork ~sw (fun () ->
      try
        Logs.debug (fun m -> m "Websocket: start thread");

        Eio.Fiber.both
          (fun () ->
            (* Send Ping continuously *)
            let rec loop () =
              Eio.Time.sleep env#clock 10.0;
              ws_send ~opcode:Ping conn "";
              loop ()
            in
            loop ())
          (fun () ->
            (* Start the process *)
            f conn)
      with e ->
        Logs.err (fun m ->
            m "Websocket: thread error: %s\n%s" (Printexc.to_string e)
              (Printexc.get_backtrace ())));
  resp
