open Util
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
  | _ -> (* Already favourited *) Lwt.return_unit
  | exception Sqlx.Error.NoRowFound ->
      let now = Ptime.now () in
      let%lwt fav =
        Db.(
          e
          @@ Favourite.(
               make ~created_at:now ~updated_at:now ~account_id:self#id
                 ~status_id ()
               |> save_one))
      in
      if%lwt Lwt.return (self#id = status#account_id) then Lwt.return_unit
      else
        let src = self in
        let%lwt dst = Db.e (Model.Account.get_one ~id:status#account_id) in
        if Db.Account.is_remote status#account then
          let%lwt activity = Activity.(like_of_favourite fav >|= like) in
          Worker.Delivery.kick ~activity ~src ~url:dst#inbox_url
        else
          Worker.Local_notify.kick
            ~activity_id:(Model.Favourite.ID.to_int fav#id)
            ~activity_type:`Favourite ~typ:`favourite ~src ~dst);%lwt

  make_status_from_model ~self_id:self#id status
  >|= yojson_of_status >>= respond_yojson
