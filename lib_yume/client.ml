let authenticator = Ca_certs.authenticator () |> Result.get_ok

let connect_via_tls url socket =
  let tls_config = Tls.Config.client ~authenticator () in
  let host =
    Uri.host url
    |> Option.map (fun x -> Domain_name.(host_exn (of_string_exn x)))
  in
  Tls_eio.client_of_flow ?host tls_config socket

let connect net ~sw url =
  let service =
    match Uri.port url with
    | Some port -> string_of_int port
    | None -> (
        match Uri.scheme url with
        | Some "ws" -> "http"
        | Some "wss" -> "https"
        | Some scheme -> scheme
        | None -> "http")
  in
  let tls_enabled = match service with "443" | "https" -> true | _ -> false in
  let host = Uri.host_with_default ~default:"localhost" url in
  let addr =
    match Eio.Net.getaddrinfo_stream net host ~service with
    | [] -> failwith "getaddrinfo failed"
    | addr :: _ -> addr
  in
  let socket = Eio.Net.connect ~sw net addr in
  let flow =
    if not tls_enabled then (socket :> Eio.Flow.two_way_ty Eio.Resource.t)
    else (connect_via_tls url socket :> Eio.Flow.two_way_ty Eio.Resource.t)
  in
  (socket, flow)

module Response = struct
  type t = { resp : Http.Response.t; body : Cohttp_eio.Body.t }

  let status { resp; _ } = Http.Response.status resp

  let headers { resp; _ } =
    resp |> Http.Response.headers |> Http.Header.to_list |> Headers.of_list

  let drain { body; _ } =
    Eio.Buf_read.(parse_exn take_all) body ~max_size:max_int
end

let request ?headers ?body ~meth env ~sw (url : string) =
  let headers = headers |> Option.map Cohttp.Header.of_list in
  let body =
    body |> Option.map (function `Fixed src -> Cohttp_eio.Body.of_string src)
  in
  let client =
    Cohttp_eio.Client.make ~https:(Some connect_via_tls) (Eio.Stdenv.net env)
  in
  let resp, body =
    Cohttp_eio.Client.call ~sw ?headers ?body client meth (Uri.of_string url)
  in
  Response.{ resp; body }

let get = request ~meth:`GET
let post = request ~meth:`POST
let put = request ~meth:`PUT
let delete = request ~meth:`DELETE

let fetch env ?(headers = []) ?(meth = `GET) ?(body = "") ?(sign = None) url =
  let path_query_fragment (u : Uri.t) =
    let res = Uri.path u in
    let res =
      match Uri.verbatim_query u with None -> res | Some q -> res ^ "?" ^ q
    in
    let res =
      match Uri.fragment u with None -> res | Some f -> res ^ "#" ^ f
    in
    res
  in
  let http_host (u : Uri.t) =
    let host = Uri.host u |> Option.get in
    match Uri.port u with
    | None -> host
    | Some port -> host ^ ":" ^ string_of_int port
  in
  let now () = Unix.gettimeofday () |> Ptime.of_float_s |> Option.get in
  let to_http_date (v : Ptime.t) : string =
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
      Ptime.to_date_time v
    in
    let month = string_of_month month in
    let day_name = Ptime.weekday v |> string_of_week in
    Printf.sprintf "%s, %02d %s %d %02d:%02d:%02d GMT" day_name day_of_month
      month year hour minute second
  in

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
      |> add (`Host, http_host uri)
      |> add (`Date, to_http_date (now ()))
    in
    let headers =
      match sign with
      | None -> headers
      | Some (priv_key, key_id, signed_headers) ->
          Signature.sign ~priv_key ~key_id ~signed_headers ~headers ~meth
            ~path:(path_query_fragment uri) ~body:(Some body)
    in
    Headers.to_list headers
  in
  try
    Eio.Switch.run @@ fun sw ->
    let resp =
      match meth with
      | `GET | `DELETE -> request env ~sw ~headers ~meth (Uri.to_string uri)
      | `POST | `PATCH ->
          request env ~sw ~headers ~body:(`Fixed body) ~meth (Uri.to_string uri)
      | _ -> failwith "Not implemented method"
    in
    let status = Response.status resp in
    Logs.debug (fun m ->
        m "[fetch] %s %s --> %s" meth_s url
          (Cohttp.Code.string_of_status status));
    let headers = Response.headers resp in
    let body = Response.drain resp in
    Ok (status, headers, body)
  with e ->
    let backtrace = Printexc.get_backtrace () in
    Logs.err (fun m ->
        m "[fetch] %s %s: %s\n%s" meth_s url (Printexc.to_string e) backtrace);
    Error ()

exception FetchFailure of (Status.t * Headers.t * string) option

let fetch_exn ?(headers = []) ?(meth = `GET) ?(body = "") ?(sign = None) env
    (url : string) : string =
  match fetch env ~headers ~meth ~body ~sign url with
  | Ok (`OK, _, body) -> body
  | Ok r -> raise (FetchFailure (Some r))
  | _ -> raise (FetchFailure None)
