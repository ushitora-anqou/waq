type stream = [ `User ]
type key = int * stream
type connection = [ `WebSocket of Httpq.Server.ws_conn ]

module Connection = struct
  type t = connection

  let compare = compare
end

module ConnectionSet = Set.Make (Connection)

let connections : (key, ConnectionSet.t) Hashtbl.t = Hashtbl.create 10
let make_key ~user_id ~stream = (user_id, stream)

let add (k : key) (conn : connection) =
  Hashtbl.find_opt connections k
  |> Option.value ~default:ConnectionSet.empty
  |> ConnectionSet.add conn
  |> Hashtbl.replace connections k

let remove (k : key) (conn : connection) =
  match Hashtbl.find_opt connections k with
  | None -> invalid_arg "Streaming.remove: key not found"
  | Some s -> s |> ConnectionSet.remove conn |> Hashtbl.replace connections k

let push ~(key : key) ~(event : string) ?payload () =
  match Hashtbl.find_opt connections key with
  | None -> () (* Just ignore *)
  | Some s ->
      s
      |> ConnectionSet.iter (function `WebSocket conn ->
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
