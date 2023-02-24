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
