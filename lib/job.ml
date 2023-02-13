open Lwt.Infix

let kick ~name (f : unit -> unit Lwt.t) : unit Lwt.t =
  let task () =
    let num_repeats = 3 in
    let sleep_duration i =
      (* Thanks to: https://github.com/mperham/sidekiq/wiki/Error-Handling *)
      (i * i * i * i) + 15 + (Random.int 10 * (i + 1)) |> float_of_int
    in
    let timeout_seconds = 25.0 in
    let rec loop i =
      try%lwt
        let timeout = Lwt_unix.sleep timeout_seconds in
        let task_done = ref false in
        Lwt.pick [ timeout; (f () >|= fun () -> task_done := true) ]
        >|= fun () -> if not !task_done then failwith "Timeout"
      with e ->
        Logq.warn (fun m ->
            m "Job failed: %s: %s: %s" name (Printexc.to_string e)
              (Printexc.get_backtrace ()));
        if i + 1 = num_repeats then (
          Logq.err (fun m -> m "Job killed: %s: Limit reached" name);
          Lwt.return_unit)
        else
          let dur = sleep_duration i in
          Logq.debug (fun m -> m "Job: %s will sleep %.1f seconds" name dur);
          Lwt_unix.sleep dur;%lwt
          loop (i + 1)
    in
    loop 0
  in
  if Config.debug_job_kick_block () then (
    Logq.debug (fun m -> m "[DEBUG ONLY] Blocked kick: %s" name);
    task ())
  else (
    Lwt.async task;
    Lwt.return_unit)
