open Entity

type res = { stream : string list; event : string; payload : string }
[@@deriving make, yojson]

let kick ~status_id ~account_id ~user_id ~stream =
  let open Lwt.Infix in
  Job.kick ~name:__FUNCTION__ @@ fun () ->
  assert (stream = `User);
  let key = Streaming.make_key ~user_id ~stream in
  let%lwt payload =
    Db.Status.get_one ~id:status_id ()
    >>= make_status_from_model ~self_id:account_id
    >|= status_to_yojson >|= Yojson.Safe.to_string
  in
  make_res ~stream:[ "user" ] ~event:"update" ~payload ()
  |> res_to_yojson |> Yojson.Safe.to_string |> Streaming.push key;
  Lwt.return_unit
