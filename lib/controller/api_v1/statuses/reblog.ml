open Util
open Helper
open Entity
open Lwt.Infix

let post req =
  let%lwt self_id = authenticate_user req in
  let status_id =
    req |> Httpq.Server.param ":id" |> int_of_string |> Model.Status.ID.of_int
  in
  let%lwt status =
    Db.(
      e @@ Status.get_one ~id:status_id ~preload:[ `account [] ] |> maybe_no_row)
    >|= function
    | Some s -> s
    | None -> Httpq.Server.raise_error_response `Not_found
  in
  let%lwt status =
    match status#reblog_of_id with
    | None -> Lwt.return status
    | Some id -> Db.e (Model.Status.get_one ~id)
  in
  let%lwt s =
    match%lwt
      Db.e
        (Model.Status.get_one ~account_id:self_id ~reblog_of_id:(Some status#id))
    with
    | s ->
        (* Already reblogged *)
        Lwt.return s
    | exception Sqlx.Error.NoRowFound ->
        let now = Ptime.now () in
        let%lwt s =
          Db.(
            e
            @@ Status.(
                 make ~text:"" ~created_at:now ~updated_at:now ~uri:""
                   ~account_id:self_id ~reblog_of_id:status#id ()
                 |> save_one_with_uri))
        in
        Worker.Distribute.kick s;%lwt
        (if s#account_id = status#account_id then Lwt.return_unit
        else if Model.Account.is_remote status#account then Lwt.return_unit
        else
          let%lwt src = Db.e (Model.Account.get_one ~id:s#account_id) in
          let%lwt dst = Db.e (Model.Account.get_one ~id:status#account_id) in
          Worker.Local_notify.kick
            ~activity_id:(Model.Status.ID.to_int s#id)
            ~activity_type:`Status ~typ:`reblog ~src ~dst);%lwt
        Lwt.return s
  in
  make_status_from_model ~self_id s
  >|= yojson_of_status >>= Helper.respond_yojson
