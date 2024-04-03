open struct
  let random_string len = Mirage_crypto_rng.generate len |> Cstruct.to_string

  let b64_encoded_sha1sum s =
    s |> Cstruct.of_string |> Mirage_crypto.Hash.SHA1.digest
    |> Cstruct.to_string |> Base64.encode_exn
end

open Websocket.Make (Cohttp_eio.Private.IO)

module Client = struct
  type 'a conn = {
    id : string;
    read_frame : unit -> Websocket.Frame.t;
    write_frame : Websocket.Frame.t option -> unit;
    socket : 'a Eio.Net.stream_socket_ty Eio.Resource.t;
  }

  let drain_handshake req ic oc nonce =
    Request.write (fun _ -> ()) req oc;
    let resp =
      match Response.read ic with
      | `Ok r -> r
      | `Eof -> raise End_of_file
      | `Invalid s -> failwith s
    in
    let status = Cohttp.Response.status resp in
    let headers = Cohttp.Response.headers resp in
    if Cohttp.Code.(is_error (code_of_status status)) then
      failwith ("error status: " ^ Cohttp.Code.(string_of_status status));
    if Cohttp.Response.version resp <> `HTTP_1_1 then
      failwith "invalid HTTP version";
    if status <> `Switching_protocols then failwith "wrong status";
    (match Cohttp.Header.get headers "upgrade" with
    | Some a when String.lowercase_ascii a = "websocket" -> ()
    | _ -> failwith "wrong upgrade");
    if not (Websocket.upgrade_present headers) then
      failwith "upgrade header not present";
    (match Cohttp.Header.get headers "sec-websocket-accept" with
    | Some accept
      when accept = b64_encoded_sha1sum (nonce ^ Websocket.websocket_uuid) ->
        ()
    | _ -> failwith "wrong accept");
    ()

  let connect' env sw url nonce extra_headers =
    (* Make request *)
    let headers =
      Cohttp.Header.add_list extra_headers
        [
          ("Upgrade", "websocket");
          ("Connection", "Upgrade");
          ("Sec-WebSocket-Key", nonce);
          ("Sec-WebSocket-Version", "13");
        ]
    in
    let req = Cohttp.Request.make ~headers url in

    (* Connect *)
    let socket, flow = Client.connect (Eio.Stdenv.net env) ~sw url in

    (* Drain handshake *)
    let ic = Eio.Buf_read.of_flow ~max_size:max_int flow in
    Eio.Buf_write.with_flow flow (fun oc -> drain_handshake req ic oc nonce);

    (socket, flow, ic)

  let connect ?(extra_headers = Cohttp.Header.init ()) ~sw env url =
    let url = Uri.of_string url in

    let nonce = Base64.encode_exn (random_string 16) in
    let socket, flow, ic = connect' env sw url nonce extra_headers in

    (* Start writer fiber. All writes must be done in this fiber,
       because Eio.Flow.write is not thread-safe.
       c.f.: https://github.com/ocaml-multicore/eio/blob/v0.11/lib_eio/flow.mli#L73-L74
    *)
    let write_queue = Eio.Stream.create 10 in
    (let rec writer () =
       try
         let frame = Eio.Stream.take write_queue in
         match frame with
         | None -> ()
         | Some frame ->
             let buf = Buffer.create 128 in
             write_frame_to_buf ~mode:(Client random_string) buf frame;
             Eio.Buf_write.with_flow flow (fun oc ->
                 Eio.Buf_write.string oc (Buffer.contents buf));
             writer ()
       with Eio.Io _ -> ()
     in
     Eio.Fiber.fork ~sw writer);

    let write_frame frame = Eio.Stream.add write_queue frame in
    let read_frame () =
      Eio.Buf_write.with_flow flow (fun oc ->
          make_read_frame ~mode:(Client random_string) ic oc ())
    in

    { socket; id = random_string 10; read_frame; write_frame }

  let id { id; _ } = id
  let read { read_frame; _ } = read_frame ()
  let write { write_frame; _ } frame = write_frame (Some frame)

  let close_transport { socket; write_frame; _ } =
    write_frame None;
    Eio.Net.close socket
end

module Server = struct
  let read_frames ic oc handler_fn =
    let read_frame = make_read_frame ~mode:Server ic oc in
    let rec inner () = read_frame () |> handler_fn |> inner in
    inner ()

  let send_frames stream oc =
    let buf = Buffer.create 128 in
    let send_frame fr =
      Buffer.clear buf;
      write_frame_to_buf ~mode:Server buf fr;
      Eio.Buf_write.string oc (Buffer.contents buf)
    in
    let rec inner () =
      match Eio.Stream.take stream with
      | None -> () (* end of stream *)
      | Some fr ->
          send_frame fr;
          inner ()
    in
    inner ()

  let upgrade_connection request incoming_handler =
    let headers = Cohttp.Request.headers request in
    let key =
      match Cohttp.Header.get headers "sec-websocket-key" with
      | Some key -> key
      | None ->
          failwith "upgrade_connection: missing header `sec-websocket-key`"
    in
    let hash = b64_encoded_sha1sum (key ^ Websocket.websocket_uuid) in
    let response_headers =
      Cohttp.Header.of_list
        [
          ("Upgrade", "websocket");
          ("Connection", "Upgrade");
          ("Sec-WebSocket-Accept", hash);
        ]
    in
    let resp =
      Cohttp.Response.make ~status:`Switching_protocols
        ~encoding:Cohttp.Transfer.Unknown ~headers:response_headers ~flush:true
        ()
    in
    let frames_out_stream = Eio.Stream.create 10 in
    let frames_out_fn = Eio.Stream.add frames_out_stream in
    let handle_conn ic oc =
      let handler = incoming_handler frames_out_fn in
      (try
         Eio.Fiber.both
           (fun () -> read_frames ic oc (fun fr -> handler (Some fr)))
           (fun () -> send_frames frames_out_stream oc)
       with
      | End_of_file -> ()
      | e ->
          Logs.err (fun m ->
              m "ws connection broken by exc: %s: %s" (Printexc.to_string e)
                (Printexc.get_backtrace ())));
      handler None
    in
    (`Expert (resp, handle_conn), frames_out_fn)
end
