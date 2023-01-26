type key = int * string

let connections : (key, [< `WebSocket of Http.ws_conn ]) Hashtbl.t =
  Hashtbl.create 10

let make_key ~user_id ~stream = (user_id, stream)

let add ~user_id ~stream = function
  | `WebSocket (conn : Http.ws_conn) ->
      let key = make_key ~user_id ~stream in
      assert (not (Hashtbl.mem connections key));
      Hashtbl.add connections key (`WebSocket conn)

let remove ~user_id ~stream = Hashtbl.remove connections (user_id, stream)

let push ~user_id ~stream msg =
  let key = make_key ~user_id ~stream in
  match Hashtbl.find_opt connections key with
  | None -> () (* Just ignore *)
  | Some (`WebSocket conn) -> Http.ws_send conn msg
