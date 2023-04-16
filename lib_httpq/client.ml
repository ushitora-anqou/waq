open Util

let fetch ?(headers = []) ?(meth = `GET) ?(body = "") ?(sign = None) url =
  let open Cohttp in
  let open Cohttp_lwt_unix in
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
    headers |> Headers.to_list |> Header.of_list
  in
  try%lwt
    let%lwt resp, body =
      match meth with
      | `GET | `DELETE -> Client.call ~headers meth uri
      | `POST | `PATCH ->
          let body = Cohttp_lwt.Body.of_string body in
          Client.call ~headers ~body meth uri
      | _ -> failwith "Not implemented method"
    in
    let status = Response.status resp in
    Logq.debug (fun m ->
        m "[fetch] %s %s --> %s" meth_s url (Code.string_of_status status));
    let headers =
      Response.headers resp |> Header.to_list
      |> List.map (fun (k, v) -> (String.lowercase_ascii k, v))
    in
    let%lwt body = Cohttp_lwt.Body.to_string body in
    Lwt.return_ok (status, headers, body)
  with e ->
    let backtrace = Printexc.get_backtrace () in
    Logq.err (fun m ->
        m "[fetch] %s %s: %s\n%s" meth_s url (Printexc.to_string e) backtrace);
    Lwt.return_error ()

exception FetchFailure of (Status.t * (string * string) list * string) option

let fetch_exn ?(headers = []) ?(meth = `GET) ?(body = "") ?(sign = None)
    (url : string) : string Lwt.t =
  match%lwt fetch ~headers ~meth ~body ~sign url with
  | Ok (`OK, _, body) -> Lwt.return body
  | Ok r -> raise (FetchFailure (Some r))
  | _ -> raise (FetchFailure None)
