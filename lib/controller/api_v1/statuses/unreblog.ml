open Helper
open Entity

let post env req =
  let self = authenticate_account req in
  let status_id =
    req |> Yume.Server.param ":id" |> int_of_string |> Model.Status.ID.of_int
  in
  try
    let status = Db.e (Model.Status.get_one ~id:status_id) in
    let reblog =
      Db.(
        e
          Model.Status.(
            get_one ~reblog_of_id:(Some status_id) ~account_id:self#id))
    in
    let entity = make_status_from_model ~self_id:self#id status in
    Worker.Removal.kick env ~account_id:self#id ~status_id:reblog#id;
    let entity =
      {
        entity with
        reblogged = false;
        reblogs_count = entity.reblogs_count - 1;
      }
    in
    yojson_of_status entity |> respond_yojson
  with Sqlx.Error.NoRowFound -> Yume.Server.raise_error_response `Not_found
