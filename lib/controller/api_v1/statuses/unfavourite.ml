open Helper
open Entity
open Lwt.Infix

let post req =
  let%lwt account_id = authenticate_user req in
  let status_id = req |> Httpq.Server.param ":id" |> int_of_string in

  let%lwt status =
    match%lwt Db.Status.get_one ~id:status_id () with
    | exception Sql.NoRowFound -> Httpq.Server.raise_error_response `Not_found
    | s -> Lwt.return s
  in

  (match%lwt Db.Favourite.get_one ~status_id ~account_id () with
  | exception Sql.NoRowFound -> (* Already unfavourited *) Lwt.return_unit
  | fav ->
      Db.Favourite.delete fav;%lwt
      if%lwt Db.Account.is_remote ~id:status.account_id then
        let%lwt src = Db.Account.get_one ~id:account_id () in
        let%lwt dst = Db.Account.get_one ~id:status.account_id () in
        let%lwt activity =
          Activity.(
            like_of_favourite fav >|= like >|= to_undo ~actor:src.uri >|= undo)
        in
        Worker.Delivery.kick ~activity ~src ~url:dst.inbox_url);%lwt

  make_status_from_model ~self_id:account_id status
  >|= yojson_of_status >>= respond_yojson
