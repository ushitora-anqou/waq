let expect_no_exn f =
  try f ()
  with e ->
    Printf.eprintf "BUG: unexpected exception: %s\n%s%!" (Printexc.to_string e)
      (Printexc.get_backtrace ());
    Unix._exit 1

let genid =
  let next_id = Atomic.make 1 in
  fun () -> Atomic.fetch_and_add next_id 1

let traceln fmt = Printf.ksprintf (fun s -> Printf.eprintf "%s\n%!" s) fmt

exception Unreachable of string

let unreachable ~__FUNCTION__:msg = raise (Unreachable msg)
