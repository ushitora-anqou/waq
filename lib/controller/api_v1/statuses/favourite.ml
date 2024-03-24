open Util
open Helper
open Entity

let post env req =
  let self = authenticate_account req in
  let status_id =
    req |> Yume.Server.param ":id" |> int_of_string |> Model.Status.ID.of_int
  in

  let status =
    try Db.e (Model.Status.get_one ~id:status_id ~preload:[ `account [] ])
    with Sqlx.Error.NoRowFound -> Yume.Server.raise_error_response `Not_found
  in

  (try Db.(e @@ Favourite.get_one ~status_id ~account_id:self#id) |> ignore
   with Sqlx.Error.NoRowFound ->
     let now = Ptime.now () in
     let fav =
       Db.(
         e
         @@ Favourite.(
              make ~created_at:now ~updated_at:now ~account_id:self#id
                ~status_id ()
              |> save_one))
     in
     if self#id = status#account_id then ()
     else
       let src = self in
       let dst = Db.e (Model.Account.get_one ~id:status#account_id) in
       if Db.Account.is_remote status#account then
         let activity = Activity.(like_of_favourite fav |> like) in
         Worker.Delivery.kick env ~activity ~src ~url:dst#inbox_url
       else
         Worker.Local_notify.kick env
           ~activity_id:(Model.Favourite.ID.to_int fav#id)
           ~activity_type:`Favourite ~typ:`favourite ~src ~dst);

  make_status_from_model ~self_id:self#id status
  |> yojson_of_status |> respond_yojson
