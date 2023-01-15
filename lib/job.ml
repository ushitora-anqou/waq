let kick ~name (f : unit -> unit Lwt.t) =
  Lwt.async @@ fun () ->
  let num_repeats = 3 in
  let sleep_duration i =
    (* Thanks to: https://github.com/mperham/sidekiq/wiki/Error-Handling *)
    (i * i * i * i) + 15 + (Random.int 10 * (i + 1)) |> float_of_int
  in
  let rec loop i =
    try%lwt f ()
    with e ->
      Log.warn (fun m -> m "Job failed: %s: %s" name (Printexc.to_string e));
      if i + 1 = num_repeats then (
        Log.err (fun m -> m "Job killed: %s: Limit reached" name);
        Lwt.return_unit)
      else
        let dur = sleep_duration i in
        Log.debug (fun m -> m "Job: %s will sleep %.1f seconds" name dur);
        Lwt_unix.sleep dur;%lwt
        loop (i + 1)
  in
  loop 0

let kick_lwt ~name (f : unit -> unit Lwt.t) =
  kick ~name f;
  Lwt.return_unit
