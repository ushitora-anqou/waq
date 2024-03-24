open Util

(*
module Throttle = struct
  type t = {
    rate : int;
    mtx : Eio.Mutex.t;
    count : (string, int) Hashtbl.t;
    mutable cleaning : bool;
  }

  let create ~rate ~n =
    {
      rate;
      mtx = Eio.Mutex.create ();
      count = Hashtbl.create n;
      cleaning = false;
    }

  let clean clock t =
    Eio.Time.sleep clock 1.0;
    Eio.Mutex.use_rw ~protect:true t.mtx (fun () ->
        Hashtbl.clear t.count;
        t.cleaning <- false)

  let wait ~sw clock t k =
    Eio.Mutex.use_rw ~protect:true t.mtx (fun () ->
        if not t.cleaning then (
          t.cleaning <- true;
          Eio.Fiber.fork ~sw (fun () -> clean clock t));

        match Hashtbl.find_opt t.count k with
        | None ->
            Hashtbl.add t.count k 1;
            true
        | Some c when c > t.rate -> false
        | Some c ->
            Hashtbl.replace t.count k (c + 1);
            true)
end

module StringHash = struct
  type t = string

  let equal = ( = )
  let hash = Hashtbl.hash
end
*)

module Uri = struct
  include Uri

  let getaddrinfo_port (u : t) =
    let scheme = Uri.scheme u |> Option.get in
    u |> Uri.port |> Option.fold ~none:scheme ~some:string_of_int

  let http_host (u : t) =
    let host = Uri.host u |> Option.get in
    match Uri.port u with
    | None -> host
    | Some port -> host ^ ":" ^ string_of_int port

  let path_query_fragment (u : t) =
    let res = Uri.path u in
    let res =
      match Uri.verbatim_query u with None -> res | Some q -> res ^ "?" ^ q
    in
    let res =
      match Uri.fragment u with None -> res | Some f -> res ^ "#" ^ f
    in
    res

  let domain (u : t) = http_host u
end

module Ptime = struct
  include Ptime

  let now () = Unix.gettimeofday () |> of_float_s |> Option.get

  let to_http_date (v : t) : string =
    let string_of_week = function
      | `Sun -> "Sun"
      | `Mon -> "Mon"
      | `Tue -> "Tue"
      | `Wed -> "Wed"
      | `Thu -> "Thu"
      | `Fri -> "Fri"
      | `Sat -> "Sat"
    in
    let string_of_month = function
      | 1 -> "Jan"
      | 2 -> "Feb"
      | 3 -> "Mar"
      | 4 -> "Apr"
      | 5 -> "May"
      | 6 -> "Jun"
      | 7 -> "Jul"
      | 8 -> "Aug"
      | 9 -> "Sep"
      | 10 -> "Oct"
      | 11 -> "Nov"
      | 12 -> "Dec"
      | _ -> assert false
    in
    let (year, month, day_of_month), ((hour, minute, second), _) =
      to_date_time v
    in
    let month = string_of_month month in
    let day_name = weekday v |> string_of_week in
    Printf.sprintf "%s, %02d %s %d %02d:%02d:%02d GMT" day_name day_of_month
      month year hour minute second
end

let fetch env ?(headers = []) ?(meth = `GET) ?(body = "") ?(sign = None) url =
  let open Yume in
  let uri = Uri.of_string url in

  (* NOTE: Ad-hoc scheme rewriting (https -> http) for localhost
     for better dev experience *)
  let uri =
    match Uri.scheme uri with
    | Some "https"
      when [ Some "localhost"; Some "127.0.0.1" ] |> List.mem (Uri.host uri) ->
        Uri.with_scheme uri (Some "http")
    | _ -> uri
  in

  let meth_s = Method.to_string meth in
  let headers =
    let headers =
      let add (k, v) headers =
        if List.mem_assoc k headers then headers else (k, v) :: headers
      in
      headers
      |> add (`Content_length, body |> String.length |> string_of_int)
      |> add (`Connection, "close")
      |> add (`Host, Uri.http_host uri)
      |> add (`Date, Ptime.(now () |> to_http_date))
    in
    let headers =
      match sign with
      | None -> headers
      | Some (priv_key, key_id, signed_headers) ->
          Signature.sign ~priv_key ~key_id ~signed_headers ~headers ~meth
            ~path:(Uri.path_query_fragment uri)
            ~body:(Some body)
    in
    Headers.to_list headers
  in
  try
    Eio.Switch.run @@ fun sw ->
    let resp =
      match meth with
      | `GET | `DELETE ->
          Client.request env ~sw ~headers ~meth (Uri.to_string uri)
      | `POST | `PATCH ->
          Client.request env ~sw ~headers ~body:(`Fixed body) ~meth
            (Uri.to_string uri)
      | _ -> failwith "Not implemented method"
    in
    let status = Client.Response.status resp in
    Logq.debug (fun m ->
        m "[fetch] %s %s --> %s" meth_s url
          (Cohttp.Code.string_of_status status));
    let headers = Client.Response.headers resp in
    let body = Client.Response.drain resp in
    Ok (status, headers, body)
  with e ->
    let backtrace = Printexc.get_backtrace () in
    Logq.err (fun m ->
        m "[fetch] %s %s: %s\n%s" meth_s url (Printexc.to_string e) backtrace);
    Error ()

exception FetchFailure of (Yume.Status.t * Yume.Headers.t * string) option

let fetch_exn ?(headers = []) ?(meth = `GET) ?(body = "") ?(sign = None) env
    (url : string) : string =
  match fetch env ~headers ~meth ~body ~sign url with
  | Ok (`OK, _, body) -> body
  | Ok r -> raise (FetchFailure (Some r))
  | _ -> raise (FetchFailure None)

module Throttle = struct
  type t = { semaphore : Eio.Semaphore.t }

  let create ~rate = { semaphore = Eio.Semaphore.make rate }

  let wait { semaphore } _k f =
    Eio.Semaphore.acquire semaphore;
    Fun.protect ~finally:(fun () -> Eio.Semaphore.release semaphore) f
end

let limitter = Throttle.create ~rate:10

let call f url =
  if Config.debug_no_throttle_fetch () then f url
  else
    let host = Uri.(of_string url |> host) |> Option.value ~default:"" in
    Throttle.wait limitter host (fun () -> f url)

let f env ?(headers = []) ?(meth = `GET) ?(body = "") ?(sign = None) =
  call (fetch env ~headers ~meth ~body ~sign)

let f_exn env ?(headers = []) ?(meth = `GET) ?(body = "") ?(sign = None) =
  call (fetch_exn env ~headers ~meth ~body ~sign)

let http_get url = f_exn url
