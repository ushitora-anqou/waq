open Util
open Helper
open Lwt.Infix

let serialize_markers xs =
  xs
  |> List.map (fun (timeline, marker) ->
         marker |> Entity.serialize_marker |> Entity.yojson_of_marker
         |> fun j -> (timeline, j))
  |> fun j -> `Assoc j

let get req =
  let%lwt user = authenticate_user req in
  Httpq.Server.query_many "timeline" req
  |> List.filter (function "home" | "notifications" -> true | _ -> false)
  |> List.sort_uniq compare
  |> Lwt_list.map_p (fun timeline ->
         Db.(
           e Marker.(get_one ~timeline ~user_id:(Some user#id)) |> maybe_no_row)
         >|= fun x ->
         let default =
           Model.Marker.make ~updated_at:(Ptime.now ()) ~last_read_id:0
             ~user_id:user#id ~timeline ()
         in
         (timeline, x |> Option.value ~default))
  >|= serialize_markers >>= respond_yojson

let post req =
  let%lwt user = authenticate_user req in
  let home_last_read_id, noti_last_read_id =
    Httpq.Server.body req |> Yojson.Safe.from_string |> expect_assoc
    |> List.fold_left
         (fun (home, noti) -> function
           | "home", `Assoc [ ("last_read_id", `String id) ] ->
               (Some (int_of_string id), noti)
           | "notifications", `Assoc [ ("last_read_id", `String id) ] ->
               (home, Some (int_of_string id))
           | _ -> raise_error_response `Bad_request)
         (None, None)
  in

  let home_marker = ref None in
  let noti_marker = ref None in
  let%lwt result =
    Db.(
      transaction @@ fun c ->
      let aux last_read_id_opt timeline out_ref =
        match last_read_id_opt with
        | None -> Lwt.return_unit
        | Some last_read_id ->
            let%lwt m =
              match%lwt Marker.get_one ~user_id:(Some user#id) ~timeline c with
              | exception Sqlx.Error.NoRowFound ->
                  Marker.make ~last_read_id ~user_id:user#id ~timeline ()
                  |> Lwt.return
              | m ->
                  m#set_last_read_id last_read_id;
                  Lwt.return m
            in
            Marker.save_one m c >|= fun m -> out_ref := Some (timeline, m)
      in
      aux home_last_read_id "home" home_marker;%lwt
      aux noti_last_read_id "notifications" noti_marker)
  in
  if not result then raise_error_response `Internal_server_error;

  [ !home_marker; !noti_marker ]
  |> List.filter_map Fun.id |> serialize_markers |> respond_yojson
