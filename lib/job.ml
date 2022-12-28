let schedule_job (factory : unit -> unit Lwt.t) : unit =
  Lwt.async @@ fun () ->
  Lwt.catch factory (fun e ->
      Log.err (fun m ->
          m "Job failed by exception: %s\n%s" (Printexc.to_string e)
            (Printexc.get_backtrace ()));
      Lwt.return_unit)

(*
let schedule_post_status (content : string) : unit =
  schedule_job @@ fun () ->
  let url = "http://localhost:3000/users/admin/inbox" in
  let%lwt res = Http.fetch ~meth:`POST ~body url in
  Lwt.return_unit
  *)
