open Util
open Helper
open Entity

let post env req =
  let self = authenticate_account req in
  let status_id =
    req |> Yume.Server.param ":id" |> int_of_string |> Model.Status.ID.of_int
  in
  let status =
    try Db.(e @@ Status.get_one ~id:status_id ~preload:[ `account [] ])
    with Sqlx.Error.NoRowFound -> Yume.Server.raise_error_response `Not_found
  in
  let status =
    match status#reblog_of_id with
    | None -> status
    | Some id -> Db.e (Model.Status.get_one ~id)
  in
  let s =
    try
      Db.e
        (Model.Status.get_one ~account_id:self#id ~reblog_of_id:(Some status#id))
    with Sqlx.Error.NoRowFound ->
      let now = Ptime.now () in
      let s =
        Db.(
          e
          @@ Status.(
               make ~text:"" ~created_at:now ~updated_at:now ~uri:""
                 ~account_id:self#id ~reblog_of_id:status#id ~spoiler_text:"" ()
               |> save_one_with_uri_and_url))
      in
      Worker.Distribute.kick env s;
      (if s#account_id = status#account_id then ()
       else if Model.Account.is_remote status#account then ()
       else
         let src = Db.e (Model.Account.get_one ~id:s#account_id) in
         let dst = Db.e (Model.Account.get_one ~id:status#account_id) in
         Worker.Local_notify.kick env
           ~activity_id:(Model.Status.ID.to_int s#id)
           ~activity_type:`Status ~typ:`reblog ~src ~dst);
      s
  in
  make_status_from_model ~self_id:self#id s
  |> yojson_of_status |> Helper.respond_yojson
