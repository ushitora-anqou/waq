open Oroutine

let () =
  let write fd buf =
    let rec loop i =
      if i = Bytes.length buf then ()
      else
        let n = write fd buf i (Bytes.length buf - i) in
        loop (i + n)
    in
    loop 0
  in
  initialize @@ fun () ->
  let fd = socket PF_INET SOCK_STREAM 0 in
  Unix.bind fd (ADDR_INET (Unix.inet_addr_of_string "127.0.0.1", 54321));
  Unix.listen fd 100;
  let num_conns = ref 0 in
  while true do
    (*traceln "accepting %d" !num_conns;*)
    num_conns := !num_conns + 1;
    let fd, _ = accept ~cloexec:true fd in
    go (fun () ->
        try
          let buf = Buffered_reader.make ~fd in
          let rec loop () =
            let line = Buffered_reader.read_until '\n' buf in
            (*traceln ">>> %s" line;*)
            if line = "\r\n" then () else loop ()
          in
          loop ();
          write fd
            (Bytes.of_string
               "HTTP/1.0 200 OK\n\
                Content-Type: text/html; charset=UTF-8\n\n\
                hello");
          Unix.close fd
        with Buffered_reader.End_of_file ->
          traceln "end of file";
          Unix.close fd)
  done
