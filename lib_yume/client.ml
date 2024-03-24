let null_auth ?ip:_ ~host:_ _ =
  Ok None (* Warning: use a real authenticator in your code! *)

let https ~authenticator =
  let tls_config = Tls.Config.client ~authenticator () in
  fun uri raw ->
    let host =
      Uri.host uri
      |> Option.map (fun x -> Domain_name.(host_exn (of_string_exn x)))
    in
    Tls_eio.client_of_flow ?host tls_config raw

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
    Cohttp_eio.Client.make
      ~https:(Some (https ~authenticator:null_auth))
      (Eio.Stdenv.net env)
  in
  let resp, body =
    Cohttp_eio.Client.call ~sw ?headers ?body client meth (Uri.of_string url)
  in
  Response.{ resp; body }

let get = request ~meth:`GET
let post = request ~meth:`POST
let put = request ~meth:`PUT
let delete = request ~meth:`DELETE
