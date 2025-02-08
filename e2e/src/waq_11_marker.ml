open Common

let f =
  make_waq_scenario @@ fun env token ->
  let Some { last_read_id = "0"; _ }, Some { last_read_id = "0"; _ } =
    get_markers env ~token `Waq [ "home"; "notifications" ]
  in
  let Some { last_read_id = "1"; _ }, None =
    post_markers env ~token `Waq [ ("home", "1") ]
  in
  let None, Some { last_read_id = "2"; _ } =
    post_markers env ~token `Waq [ ("notifications", "2") ]
  in
  let Some { last_read_id = "1"; _ }, Some { last_read_id = "2"; _ } =
    get_markers env ~token `Waq [ "home"; "notifications" ]
  in
  ()
[@@warning "-8"]
