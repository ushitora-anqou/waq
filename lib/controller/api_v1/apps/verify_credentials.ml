type res = { name : string } [@@deriving make, yojson]

let get req =
  let%lwt token = Helper.authenticate_bearer req in
  let%lwt app =
    Db.(e OAuthApplication.(get_one ~id:(Option.get token#application_id)))
  in
  make_res ~name:app#name |> yojson_of_res |> Yojson.Safe.to_string
  |> Httpq.Server.respond
