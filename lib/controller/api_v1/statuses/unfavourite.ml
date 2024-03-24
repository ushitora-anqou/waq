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

  (match Db.(e @@ Favourite.get_one ~status_id ~account_id:self#id) with
  | exception Sqlx.Error.NoRowFound -> (* Already unfavourited *) ()
  | fav ->
      Db.(e @@ Favourite.delete [ fav ]);
      if Model.Account.is_remote status#account then
        let src = self in
        let dst = Db.e (Model.Account.get_one ~id:status#account_id) in
        let activity =
          Activity.(
            like_of_favourite fav |> like |> to_undo ~actor:src#uri |> undo)
        in
        Worker.Delivery.kick env ~activity ~src ~url:dst#inbox_url);

  make_status_from_model ~self_id:self#id status
  |> yojson_of_status |> respond_yojson
