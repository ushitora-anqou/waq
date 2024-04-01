module Runner = struct
  type t = { chan : (unit -> unit) Eio.Stream.t }

  let global_runner = { chan = Eio.Stream.create 0 }

  let start_global_runner ~sw =
    Eio.Fiber.fork ~sw (fun () ->
        let rec loop () =
          let task = Eio.Stream.take global_runner.chan in
          Eio.Fiber.fork ~sw task;
          loop ()
        in
        loop ())

  let queue task = Eio.Stream.add global_runner.chan task
end

let kick env ~name (f : unit -> unit) : unit =
  let timeout_seconds = 25.0 in

  if Config.debug_job_kick_block () then (
    Logs.debug (fun m -> m "[DEBUG ONLY] Blocked kick: %s" name);
    try Eio.Time.with_timeout_exn env#clock timeout_seconds f
    with e ->
      Logs.warn (fun m ->
          m "[DEBUG ONLY] Fail immediately due to job failure:\n%s: %s: %s" name
            (match e with _ -> Printexc.to_string e)
            (Printexc.get_backtrace ()));
      raise e)
  else
    let task () =
      let num_repeats = 3 in
      let sleep_duration i =
        (* Thanks to: https://github.com/mperham/sidekiq/wiki/Error-Handling *)
        (i * i * i * i) + 15 + (Random.int 10 * (i + 1)) |> float_of_int
      in
      let rec loop i =
        try Eio.Time.with_timeout_exn env#clock timeout_seconds f
        with e ->
          Logs.warn (fun m ->
              m "Job failed: %s: %s: %s" name (Printexc.to_string e)
                (Printexc.get_backtrace ()));
          if i + 1 = num_repeats then
            Logs.err (fun m -> m "Job killed: %s: Limit reached" name)
          else
            let dur = sleep_duration i in
            Logs.debug (fun m -> m "Job: %s will sleep %.1f seconds" name dur);
            Eio.Time.sleep env#clock dur;
            loop (i + 1)
      in
      loop 0
    in
    Runner.queue task;
    ()
