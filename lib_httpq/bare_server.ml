open Util
open Lwt.Infix

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
      Logq.err (fun m ->
          m "Unexpected exception: %s %s: %s\n%s" (Method.to_string meth)
            (Uri.to_string uri) (Printexc.to_string e)
            (Printexc.get_backtrace ()));
      respond ~status:`Internal_server_error ~body:"" ~headers:[]
  in
  let server =
    Cohttp_lwt_unix.Server.make_response_action ~callback ()
    |> Cohttp_lwt_unix.Server.create ~mode:(`TCP (`Port port))
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
            Logq.debug (fun m -> m "Websocket: recv Close");
            ws_send ~opcode:Close conn "";
            conn.closed <- true;
            recv_stream_push None
        | Text | Binary ->
            Logq.debug (fun m -> m "Websocket: recv: %s" content);
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
        Logq.debug (fun m -> m "Websocket: start thread");
        f conn
      with e ->
        Logq.err (fun m ->
            m "Websocket: thread error: %s\n%s" (Printexc.to_string e)
              (Printexc.get_backtrace ()));
        Lwt.return_unit);
  Lwt.return resp
