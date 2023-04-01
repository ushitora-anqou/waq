open Helper
open Entity

let post req =
  let%lwt self = authenticate_account req in
  let status_id =
    req |> Httpq.Server.param ":id" |> int_of_string |> Model.Status.ID.of_int
  in
  try%lwt
    let%lwt status = Db.e (Model.Status.get_one ~id:status_id) in
    let%lwt reblog =
      Db.e (Model.Status.get_one ~reblog_of_id:(Some status_id))
    in
    let%lwt entity = make_status_from_model ~self_id:self#id status in
    Worker.Removal.kick ~account_id:self#id ~status_id:reblog#id;%lwt
    let entity =
      {
        entity with
        reblogged = false;
        reblogs_count = entity.reblogs_count - 1;
      }
    in
    yojson_of_status entity |> respond_yojson
  with Sqlx.Error.NoRowFound -> Httpq.Server.raise_error_response `Not_found
