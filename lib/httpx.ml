type request_body =
  | JSON of Yojson.Safe.t
  | Form of (string * string list) list

type request = {
  http_request : Http.request;
  body : request_body;
  raw_body : string;
  headers : (string * string) list;
}
[@@deriving make]

let param name (r : request) = Http.param name r.http_request
let body (r : request) = r.raw_body

let query ?default name (r : request) =
  try
    match r.body with
    | JSON (`Assoc l) -> (
        match List.assoc name l with
        | `Bool b -> string_of_bool b
        | `Int i -> string_of_int i
        | `String s -> s
        | _ -> failwith "json assoc")
    | JSON _ -> failwith "json"
    | Form body -> (
        match Http.query_opt name r.http_request with
        | Some l -> List.hd l
        | None -> body |> List.assoc name |> List.hd)
  with
  | _ when default <> None -> Option.get default
  | _ -> Http.raise_error_response `Bad_request

let query_opt name (r : request) = try Some (query name r) with _ -> None

let int_of_string s =
  match int_of_string_opt s with
  | None -> Http.raise_error_response `Bad_request
  | Some i -> i

let bool_of_string s =
  match bool_of_string_opt s with
  | None -> Http.raise_error_response `Bad_request
  | Some b -> b

let call ~meth target f =
  Http.call meth target @@ fun req ->
  let%lwt raw_body = Http.body req in
  let headers =
    Http.headers req |> List.map (fun (k, v) -> (String.lowercase_ascii k, v))
  in
  let body =
    if raw_body = "" then Form []
    else
      match List.assoc_opt "content-type" headers with
      | Some "application/json" -> JSON (Yojson.Safe.from_string raw_body)
      | Some "application/x-www-form-urlencoded" | _ ->
          Form (raw_body |> Uri.query_of_encoded)
  in
  let req = make_request ~http_request:req ~raw_body ~body ~headers () in
  f req

let get = call ~meth:`GET
let post = call ~meth:`POST
let options = call ~meth:`OPTIONS

let authenticate_bearer (r : request) =
  try
    let header = r.headers |> List.assoc "authorization" in
    assert (String.starts_with ~prefix:"Bearer " header);
    let bearer_token = String.sub header 7 (String.length header - 7) in
    Oauth.authenticate_access_token bearer_token
  with _ -> Http.raise_error_response `Unauthorized

let authenticate_user (r : request) =
  try
    let%lwt token = authenticate_bearer r in
    token.resource_owner_id |> Option.get |> Lwt.return
  with _ -> Http.raise_error_response `Unauthorized

let websocket (r : request) = Http.websocket r.http_request

module Cors = struct
  type t = { target : string; methods : Http.Method.t list; origin : string }

  let make target ?(origin = "*") ~methods () = { target; methods; origin }
end

let router ~cors routes =
  let cors_routes =
    let handler (r : Cors.t) req =
      let headers =
        [
          ("access-control-allow-origin", r.origin);
          ( "access-control-allow-methods",
            r.methods |> List.map Http.Method.to_string |> String.concat ", " );
        ]
      in
      let headers =
        req.headers
        |> List.assoc_opt "access-control-request-headers"
        |> Option.fold ~none:headers ~some:(fun v ->
               ("access-control-allow-headers", v) :: headers)
      in
      Http.respond ~status:`No_content ~headers ""
    in
    cors |> List.map (fun (r : Cors.t) -> options r.target (handler r))
  in
  Http.router (routes @ cors_routes)
