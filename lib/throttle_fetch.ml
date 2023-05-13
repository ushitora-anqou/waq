open Util
open Lwt.Infix

module StringHash = struct
  type t = string

  let equal = ( = )
  let hash = Hashtbl.hash
end

module StringLwtThrottle = Lwt_throttle.Make (StringHash)

let limitter = StringLwtThrottle.create ~rate:1 ~max:100 ~n:1

let call f url =
  let host = Uri.(of_string url |> host) |> Option.value ~default:"" in
  let rec aux () =
    if%lwt StringLwtThrottle.wait limitter host then f url
    else (
      Lwt_unix.sleep 1.0;%lwt
      aux ())
  in
  aux ()

let f ?(headers = []) ?(meth = `GET) ?(body = "") ?(sign = None) =
  call (Httpq.Client.fetch ~headers ~meth ~body ~sign)

let f_exn ?(headers = []) ?(meth = `GET) ?(body = "") ?(sign = None) =
  call (Httpq.Client.fetch_exn ~headers ~meth ~body ~sign)

type curl_config = {
  followlocation : bool; [@default false]
  maxredirs : int; [@default 1]
}
[@@deriving make]

let curl' (c : curl_config) url =
  let open Curl in
  let h = init () in
  set_url h url;
  set_useragent h "Curl_lwt";
  set_nosignal h true;
  set_connecttimeout h 5;
  set_timeout h 10;
  set_followlocation h c.followlocation;
  set_maxredirs h c.maxredirs;
  set_ipresolve h IPRESOLVE_V4;
  set_encoding h CURL_ENCODING_ANY;

  (* Handle Set-Cookie header when redirect *)
  set_cookiefile h "";

  Lwt.finalize
    (fun () ->
      let b = Buffer.create 16 in
      set_writefunction h (fun s ->
          (* FIXME: Return CURL_WRITEFUNC_ERROR if s is too large *)
          Buffer.add_string b s;
          String.length s);
      Logq.debug (fun m -> m "[curl] GET %s" url);
      match%lwt Curl_lwt.perform h with
      | CURLE_OK -> Lwt.return_ok (Buffer.contents b)
      | code ->
          let err_msg = strerror code in
          Logq.err (fun m -> m "[curl] Failed GET %s: %s" url err_msg);
          Lwt.return_error err_msg
      | exception CurlException (code, i, s) ->
          let err_msg = strerror code in
          Logq.err (fun m ->
              m "[curl] Failed by exception CurlException GET %s: %s: %d: %s"
                url err_msg i s);
          Lwt.return_error err_msg
      | exception NotImplemented s ->
          Logq.err (fun m ->
              m "[curl] Failed by exception NotImplemented GET %s: %s" url s);
          Lwt.return_error s)
    (fun () ->
      cleanup h;
      Lwt.return ())

let curl_exn' c url =
  curl' c url >|= function Ok x -> x | Error s -> failwith s

let curl = call *< curl'
let curl_exn = call *< curl_exn'
let http_get = curl_exn (make_curl_config ~followlocation:true ())
