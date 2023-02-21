open Common

type agent = {
  kind : [ `Waq | `Mstdn ];
  token : string;
  username : string;
  domain : string;
}
[@@deriving make]

let lookup src = lookup src.kind ~token:src.token
let follow src = follow src.kind ~token:src.token
let post src = post src.kind ~token:src.token
let search src = search src.kind ~token:src.token
let reblog src = reblog src.kind ~token:src.token
let home_timeline src = home_timeline src.kind ~token:src.token
let delete_status src = delete_status src.kind ~token:src.token
let get_status src = get_status src.kind ~token:src.token

let lookup_agent src dst =
  let domain = if src.domain = dst.domain then None else Some dst.domain in
  lookup src ~username:dst.username ?domain ()

let follow_agent src dst =
  let%lwt id, _, _ = lookup_agent src dst in
  follow src id

let expect_no_status src id =
  try%lwt
    get_status src id |> ignore_lwt;%lwt
    assert false
  with Httpq.Client.FetchFailure (Some (`Not_found, _, _)) -> Lwt.return_unit

let f (a0 : agent) (a1 : agent) =
  (* a0: Follow a1 *)
  follow_agent a0 a1;%lwt
  Lwt_unix.sleep 1.0;%lwt

  (* a1: Post *)
  let%lwt { uri; id = a1_post_id; _ } = post a1 () in
  Lwt_unix.sleep 1.0;%lwt

  (* a0: Get the post id *)
  let%lwt a0_post_id =
    match%lwt search a0 uri with
    | _, [ s ], _ -> Lwt.return s.id
    | _ -> assert false
  in

  (* a0: Reblog the post *)
  let%lwt a0_reblog_id = reblog a0 ~id:a0_post_id >|= fun r -> r.id in
  Lwt_unix.sleep 1.0;%lwt

  (* a0: Check the posts *)
  let%lwt _ = get_status a0 a0_post_id in
  let%lwt _ = get_status a0 a0_reblog_id in

  (* a1: Delete the post *)
  delete_status a1 a1_post_id |> ignore_lwt;%lwt
  Lwt_unix.sleep 1.0;%lwt

  (* a0: Check the posts *)
  expect_no_status a0 a0_post_id;%lwt
  expect_no_status a0 a0_reblog_id;%lwt

  Lwt.return_unit

let f_waq_mstdn =
  make_waq_and_mstdn_scenario @@ fun waq_token mstdn_token ->
  let a0 =
    make_agent ~kind:`Waq ~token:waq_token ~username:"user1"
      ~domain:waq_server_domain
  in
  let a1 =
    make_agent ~kind:`Mstdn ~token:mstdn_token ~username:"admin"
      ~domain:"localhost:3000"
  in
  f a0 a1;%lwt
  Lwt.return_unit

let f_mstdn_waq =
  make_waq_and_mstdn_scenario @@ fun waq_token mstdn_token ->
  let a0 =
    make_agent ~kind:`Mstdn ~token:mstdn_token ~username:"admin"
      ~domain:"localhost:3000"
  in
  let a1 =
    make_agent ~kind:`Waq ~token:waq_token ~username:"user1"
      ~domain:waq_server_domain
  in
  f a0 a1;%lwt
  Lwt.return_unit
