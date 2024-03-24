type res = { name : string } [@@deriving make, yojson]

let get _ req =
  let token = Helper.authenticate_bearer req in
  let app =
    Db.(e OAuthApplication.(get_one ~id:(Option.get token#application_id)))
  in
  make_res ~name:app#name |> yojson_of_res |> Yojson.Safe.to_string
  |> Yume.Server.respond
