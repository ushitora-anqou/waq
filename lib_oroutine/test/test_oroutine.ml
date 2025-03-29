open Oroutine

let ( *> ) f g a = g (f a)

let write fd buf =
  let rec loop i =
    if i = Bytes.length buf then ()
    else
      let n = write fd buf i (Bytes.length buf - i) in
      loop (i + n)
  in
  loop 0

let test_select_case1 () =
  initialize @@ fun () ->
  let ch = Chan.make 2 in
  Chan.send 42 ch;
  Chan.send 10 ch;
  assert (Chan.recv ch |> Result.get_ok = 42);
  assert (Chan.recv ch |> Result.get_ok = 10);
  Chan.send 42 ch;
  assert (Chan.recv ch |> Result.get_ok = 42);
  Chan.send 10 ch;
  assert (Chan.recv ch |> Result.get_ok = 10);
  ()

let test_select_case2 () =
  initialize @@ fun () ->
  let ch1 = Chan.make 1 in
  let ch2 = Chan.make 1 in
  go (fun () ->
      assert (Chan.recv ch1 |> Result.get_ok = 1);
      Chan.send 2 ch2;
      assert (Chan.recv ch1 |> Result.get_ok = 3);
      Chan.send 4 ch2);
  Chan.send 1 ch1;
  assert (Chan.recv ch2 |> Result.get_ok = 2);
  Chan.send 3 ch1;
  assert (Chan.recv ch2 |> Result.get_ok = 4);
  ()

let test_select_case3 () =
  initialize @@ fun () ->
  let ch1 = Chan.make 1 in
  let ch2 = Chan.make 1 in
  go (fun () ->
      sleep_s 0.01;
      Chan.send 1 ch1);
  let v =
    Chan.select
      [
        Recv (ch1, fun i -> 10 + Result.get_ok i);
        Recv (ch2, fun i -> 20 + Result.get_ok i);
      ]
  in
  assert (v = 11);
  ()

let test_select_case4 () =
  initialize @@ fun () ->
  let ch1 = Chan.make 1 in
  let ch2 = Chan.make 1 in
  Chan.send 101 ch1;
  Chan.send 102 ch2;
  go (fun () ->
      sleep_s 0.01;
      Chan.recv ch1 |> ignore);
  Chan.select [ Send (ch1, 1, Fun.const ()); Send (ch2, 2, Fun.const ()) ];
  assert (Chan.recv ch1 |> Result.get_ok = 1);
  assert (Chan.recv ch2 |> Result.get_ok = 102);
  ()

let test_select_case5 () =
  initialize @@ fun () ->
  let ch_i = Chan.make 1 in
  let ch_s = Chan.make 1 in
  go (fun () ->
      sleep_s 0.02;
      Chan.send 1 ch_i);
  go (fun () ->
      sleep_s 0.01;
      Chan.send "a" ch_s);
  let s =
    Chan.select
      [
        Recv (ch_i, Result.get_ok *> string_of_int);
        Recv (ch_s, Result.get_ok *> Fun.id);
      ]
  in
  assert (s = "a");
  let s =
    Chan.select
      [
        Recv (ch_i, Result.get_ok *> string_of_int);
        Recv (ch_s, Result.get_ok *> Fun.id);
      ]
  in
  assert (s = "1");
  ()

let test_basics_case1 () =
  assert (initialize (fun () -> 10) = 10);
  ()

let test_basics_case2 () =
  initialize @@ fun () ->
  let v = Atomic.make 0 in
  let launch i =
    go (fun () ->
        let rec loop () =
          if Atomic.compare_and_set v i (i + 1) then ()
          else (
            yield ();
            loop ())
        in
        loop ())
  in
  let n = 100 in
  for i = 0 to n - 1 do
    launch i
  done;
  let rec loop () =
    if Atomic.get v = n then ()
    else (
      yield ();
      loop ())
  in
  loop ()

let test_net_case1 () =
  initialize @@ fun () ->
  let start_tcp_echo_server () =
    let handler fd () =
      let buf = Buffered_reader.make ~fd in
      try
        while true do
          let line = Buffered_reader.read_until '\n' buf in
          write fd (Bytes.of_string line)
        done
      with Buffered_reader.End_of_file -> Unix.close fd
    in
    let fd = socket PF_INET SOCK_STREAM 0 in
    Unix.(bind fd (ADDR_INET (inet_addr_of_string "127.0.0.1", 0)));
    let sa = Unix.getsockname fd in
    Unix.listen fd 10;
    go (fun () ->
        while true do
          let fd', _ = accept ~cloexec:true fd in
          go (handler fd')
        done);
    sa
  in
  let sa = start_tcp_echo_server () in
  let fd = socket (Unix.domain_of_sockaddr sa) Unix.SOCK_STREAM 0 in
  connect fd sa;
  let s = "hello\n" in
  write fd (Bytes.of_string s);
  write fd (Bytes.of_string s);
  write fd (Bytes.of_string s);
  let recv_buf = Buffered_reader.make ~fd in
  let s1 = Buffered_reader.read_until '\n' recv_buf in
  let s2 = Buffered_reader.read_until '\n' recv_buf in
  let s3 = Buffered_reader.read_until '\n' recv_buf in
  assert (s = s1 && s = s2 && s = s3);
  ()

let test_channel_close_case1 () =
  initialize @@ fun () ->
  let ch = Chan.make 1 in
  Chan.close ch;
  assert (Chan.recv ch = Error `Closed);
  ()

let test_channel_close_case2 () =
  initialize @@ fun () ->
  let ch1 = Chan.make 1 in
  let ch2 = Chan.make 1 in
  Chan.close ch1;
  let v = Chan.select [ Recv (ch1, fun _ -> 1); Recv (ch2, fun _ -> 2) ] in
  assert (v = 1);
  ()

let () =
  let open Alcotest in
  run "oroutine"
    [
      (*
      ( "entrypoint",
        [ test_case "entrypoint" `Quick (fun () -> Oroutine.entrypoint ()) ] );
        *)
      ( "select",
        [
          test_case "case 1" `Quick test_select_case1;
          test_case "case 2" `Quick test_select_case2;
          test_case "case 3" `Quick test_select_case3;
          test_case "case 4" `Quick test_select_case4;
          test_case "case 5" `Quick test_select_case5;
        ] );
      ( "basics",
        [
          test_case "case 1" `Quick test_basics_case1;
          test_case "case 2" `Quick test_basics_case2;
        ] );
      ("net", [ test_case "case 1" `Quick test_net_case1 ]);
      ( "channel close",
        [
          test_case "case 1" `Quick test_channel_close_case1;
          test_case "case 2" `Quick test_channel_close_case2;
        ] );
    ]
