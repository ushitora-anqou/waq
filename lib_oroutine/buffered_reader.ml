module Make (R : Runtime.S) = struct
  exception End_of_file

  type t = {
    buf : bytes;
    mutable buf_i : int;
    mutable buf_size : int;
    fd : Unix.file_descr;
  }

  let make ~fd =
    let buf_size = 100 in
    { buf = Bytes.create buf_size; buf_i = 0; buf_size = 0; fd }

  let read_char r =
    if r.buf_i = r.buf_size then (
      let n = R.read r.fd r.buf 0 (Bytes.length r.buf) in
      if n = 0 then raise End_of_file;
      r.buf_i <- 0;
      r.buf_size <- n);
    r.buf_i <- r.buf_i + 1;
    Bytes.get r.buf (r.buf_i - 1)

  let read_until sep r =
    let buf = Buffer.create 0 in
    let rec loop () =
      let ch = read_char r in
      Buffer.add_char buf ch;
      if ch = sep then Buffer.contents buf else loop ()
    in
    loop ()
end
