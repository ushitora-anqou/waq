type t = { fd_read : Unix.file_descr; fd_write : Unix.file_descr }

let make () =
  let fd_read, fd_write = Unix.pipe ~cloexec:true () in
  Unix.set_nonblock fd_read;
  Unix.set_nonblock fd_write;
  { fd_read; fd_write }

let signal =
  let signal_bytes = Bytes.of_string "!" in
  fun efd ->
    try ignore @@ Unix.single_write efd.fd_write signal_bytes 0 1
    with Unix.(Unix_error ((EAGAIN | EWOULDBLOCK | EPIPE), _, _)) -> ()

let fd_read_to_poll efd = efd.fd_read

let clear =
  let clear_buf = Bytes.create 8 in
  fun efd ->
    let rec loop () =
      match Unix.read efd.fd_read clear_buf 0 (Bytes.length clear_buf) with
      | _ -> loop ()
      | exception Unix.(Unix_error ((EAGAIN | EWOULDBLOCK | EPIPE), _, _)) -> ()
    in
    loop ()
