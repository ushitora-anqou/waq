open Helper

let get req =
  (* FIXME: Support ?page and ?min_id *)
  let username = Httpq.Server.param ":name" req in
  let%lwt acct = Db.Account.get_one ~username ~domain:None () in
  let id = acct.outbox_url in
  let%lwt totalItems = Db.count_statuses ~account_id:acct.id in
  let first = acct.outbox_url ^ "?page=true" in
  let last = acct.outbox_url ^ "?min_id=0&page=true" in
  Activity.(
    make_ordered_collection ~id ~totalItems ~first ~last
    |> ordered_collection |> to_yojson)
  |> respond_yojson
