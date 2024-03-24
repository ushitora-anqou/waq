open Entity

let kick env ~status_id ~account_id ~user_id ~stream =
  Job.kick env ~name:__FUNCTION__ @@ fun () ->
  assert (stream = `User);
  let key = Streaming.make_key ~user_id ~stream in
  let payload =
    Db.e (Model.Status.get_one ~id:status_id)
    |> make_status_from_model ~self_id:account_id
    |> yojson_of_status |> Yojson.Safe.to_string
  in
  Streaming.push ~key ~event:"update" ~payload ();
  ()
