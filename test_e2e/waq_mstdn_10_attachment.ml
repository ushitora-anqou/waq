open Common2

let test_image =
  {|
iVBORw0KGgoAAAANSUhEUgAAADIAAAAyAQAAAAA2RLUcAAAABGdBTUEAALGPC/xhBQAAACBjSFJN
AAB6JgAAgIQAAPoAAACA6AAAdTAAAOpgAAA6mAAAF3CculE8AAAAAmJLR0QAAd2KE6QAAAAHdElN
RQfnAxYCJTrYPC4yAAAADklEQVQY02NgGAWDCQAAAZAAAcWb20kAAAAldEVYdGRhdGU6Y3JlYXRl
ADIwMjMtMDMtMjJUMDI6Mzc6NTgrMDA6MDClQ3CPAAAAJXRFWHRkYXRlOm1vZGlmeQAyMDIzLTAz
LTIyVDAyOjM3OjU4KzAwOjAw1B7IMwAAAABJRU5ErkJggg==|}
  |> String.trim |> String.split_on_char '\n' |> String.concat ""
  |> Base64.decode_exn

let f (a0 : agent) (a1 : agent) =
  (* a0: Follow a1 *)
  follow_agent a0 a1;%lwt
  Lwt_unix.sleep 1.0;%lwt

  (* a1: Post with attachments *)
  let%lwt { id = media_id; _ } =
    upload_media a1 ~filename:"test0.png" ~data:test_image
      ~content_type:"image/png"
  in
  let%lwt { id = media_id2; _ } =
    upload_media a1 ~filename:"test1.png" ~data:test_image
      ~content_type:"image/png"
  in
  let%lwt { uri; _ } = post a1 ~media_ids:[ media_id; media_id2 ] () in
  Lwt_unix.sleep 1.0;%lwt

  (* a0: Get the post *)
  let%lwt a0_post =
    match%lwt search a0 uri with
    | _, [ s ], _ -> Lwt.return s
    | _ -> assert false
  in

  (* a0: Check the post *)
  let ats = a0_post.media_attachments in
  assert (List.length ats = 2);
  assert (ats |> List.for_all (fun (a : media_attachment) -> a.type_ = "image"));

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
