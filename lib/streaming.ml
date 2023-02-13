type stream = [ `User ]
type key = int * stream
type connection = [ `WebSocket of Httpq.Server.ws_conn ]
type connection_id = int

let connections : (key, (connection_id, connection) Hashtbl.t) Hashtbl.t =
  Hashtbl.create 10

let make_key ~user_id ~stream = (user_id, stream)

let add : key -> connection -> connection_id =
  let id = ref 0 in
  fun k conn ->
    id := !id + 1;
    let h =
      match Hashtbl.find_opt connections k with
      | Some h -> h
      | None ->
          let h = Hashtbl.create 1 in
          Hashtbl.add connections k h;
          h
    in
    Hashtbl.add h !id conn;
    !id

let remove (k : key) (id : connection_id) =
  match Hashtbl.find_opt connections k with
  | None -> invalid_arg "Streaming.remove: key not found"
  | Some h -> (
      match Hashtbl.find_opt h id with
      | None -> invalid_arg "Streaming.remove: connection id not found"
      | Some _ ->
          Hashtbl.remove h id;
          if Hashtbl.length h = 0 then Hashtbl.remove connections k)

let push ~(key : key) ~(event : string) ?payload () =
  match Hashtbl.find_opt connections key with
  | None -> () (* Just ignore *)
  | Some h ->
      h
      |> Hashtbl.iter (fun _id -> function
           | `WebSocket conn ->
               let _, stream = key in
               let stream = match stream with `User -> "user" in
               let l =
                 [
                   ("stream", `List [ `String stream ]); ("event", `String event);
                 ]
               in
               let l =
                 payload
                 |> Option.fold ~none:l ~some:(fun payload ->
                        ("payload", `String payload) :: l)
               in
               `Assoc l |> Yojson.Safe.to_string |> Httpq.Server.ws_send conn)
