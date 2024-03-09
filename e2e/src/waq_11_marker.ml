open Common

let f =
  make_waq_scenario @@ fun token ->
  let%lwt Some { last_read_id = "0"; _ }, Some { last_read_id = "0"; _ } =
    get_markers ~token `Waq [ "home"; "notifications" ]
  in
  let%lwt Some { last_read_id = "1"; _ }, None =
    post_markers ~token `Waq [ ("home", "1") ]
  in
  let%lwt None, Some { last_read_id = "2"; _ } =
    post_markers ~token `Waq [ ("notifications", "2") ]
  in
  let%lwt Some { last_read_id = "1"; _ }, Some { last_read_id = "2"; _ } =
    get_markers ~token `Waq [ "home"; "notifications" ]
  in
  Lwt.return_unit
  [@@warning "-8"]
