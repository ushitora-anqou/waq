open Util
open Helper

let get_followers _env req =
  (* FIXME: Support ?page and ?min_id *)
  let username = Yume.Server.param ":name" req in
  let acct = Db.e (Model.Account.get_one ~username ~domain:None) in
  let id = acct#followers_url in
  let totalItems = Db.(e @@ count_followers ~account_id:acct#id) in
  let first = acct#followers_url ^ "?page=1" in
  Activity.(
    make_ordered_collection ~id ~totalItems ~first ()
    |> ordered_collection |> to_yojson)
  |> respond_yojson

let get_following _env req =
  (* FIXME: Support ?page and ?min_id *)
  let username = Yume.Server.param ":name" req in
  let acct = Db.e (Model.Account.get_one ~username ~domain:None) in
  let id = acct#uri ^/ "following" in
  let totalItems = Db.(e @@ count_following ~account_id:acct#id) in
  let first = id ^ "?page=1" in
  Activity.(
    make_ordered_collection ~id ~totalItems ~first ()
    |> ordered_collection |> to_yojson)
  |> respond_yojson
