open Helper

let get _ req =
  (* FIXME: Support ?page and ?min_id *)
  let username = Yume.Server.param ":name" req in
  let acct = Db.e (Model.Account.get_one ~username ~domain:None) in
  let id = acct#outbox_url in
  let totalItems = Db.(e @@ Status.count ~account_id:(`Eq acct#id)) in
  let first = acct#outbox_url ^ "?page=true" in
  let last = acct#outbox_url ^ "?min_id=0&page=true" in
  Activity.(
    make_ordered_collection ~id ~totalItems ~first ~last ()
    |> ordered_collection |> to_yojson)
  |> respond_yojson
