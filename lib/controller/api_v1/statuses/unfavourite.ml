open Helper
open Entity
open Lwt.Infix

let post req =
  let%lwt self = authenticate_account req in
  let status_id =
    req |> Httpq.Server.param ":id" |> int_of_string |> Model.Status.ID.of_int
  in

  let%lwt status =
    match%lwt
      Db.e (Model.Status.get_one ~id:status_id ~preload:[ `account [] ])
    with
    | exception Sqlx.Error.NoRowFound ->
        Httpq.Server.raise_error_response `Not_found
    | s -> Lwt.return s
  in

  (match%lwt Db.(e @@ Favourite.get_one ~status_id ~account_id:self#id) with
  | exception Sqlx.Error.NoRowFound ->
      (* Already unfavourited *) Lwt.return_unit
  | fav ->
      Db.(e @@ Favourite.delete [ fav ]);%lwt
      if%lwt Model.Account.is_remote status#account |> Lwt.return then
        let src = self in
        let%lwt dst = Db.e (Model.Account.get_one ~id:status#account_id) in
        let%lwt activity =
          Activity.(
            like_of_favourite fav >|= like >|= to_undo ~actor:src#uri >|= undo)
        in
        Worker.Delivery.kick ~activity ~src ~url:dst#inbox_url);%lwt

  make_status_from_model ~self_id:self#id status
  >|= yojson_of_status >>= respond_yojson
