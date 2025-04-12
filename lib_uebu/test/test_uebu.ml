module Fake_reader = struct
  type t = { src : string; mutable pos : int }
  type error = [ `Eof ]

  let make ~src = { src; pos = 0 }

  let peek_char r =
    if r.pos >= String.length r.src then Error `Eof else Ok r.src.[r.pos]

  let pop_char r =
    if r.pos >= String.length r.src then Error `Eof
    else (
      r.pos <- r.pos + 1;
      Ok r.src.[r.pos - 1])
end

module Rfc1945 = Uebu.Rfc1945 (Fake_reader)
module R = Rfc1945.R

let test_r_octet () =
  let reader = Fake_reader.make ~src:"ab\xff" in
  let r = R.make ~reader in
  assert (R.peek_octet r = Ok 'a');
  assert (R.pop_octet r = Ok 'a');
  assert (R.peek_octet r = Ok 'b');
  assert (R.pop_octet r = Ok 'b');
  assert (R.peek_octet r = Ok '\xff');
  assert (R.pop_octet r = Ok '\xff');
  assert (R.peek_octet r = Error `Eof);
  assert (R.pop_octet r = Error `Eof);
  ()

let test_r_char () =
  let reader = Fake_reader.make ~src:"ab" in
  let r = R.make ~reader in
  assert (R.peek_char r = Ok 'a');
  assert (R.pop_char r = Ok 'a');
  assert (R.peek_char r = Ok 'b');
  assert (R.pop_char r = Ok 'b');
  assert (R.peek_char r = Error `Eof);
  assert (R.pop_char r = Error `Eof);

  let reader = Fake_reader.make ~src:"ab\xff" in
  let r = R.make ~reader in
  assert (R.pop_char r = Ok 'a');
  assert (R.pop_char r = Ok 'b');
  assert (R.peek_char r = Error `Unexpected_char);
  assert (R.pop_char r = Error `Unexpected_char);
  ()

let test_r_upalpha () =
  let reader = Fake_reader.make ~src:"AB" in
  let r = R.make ~reader in
  assert (R.peek_upalpha r = Ok 'A');
  assert (R.pop_upalpha r = Ok 'A');
  assert (R.peek_upalpha r = Ok 'B');
  assert (R.pop_upalpha r = Ok 'B');
  assert (R.peek_upalpha r = Error `Eof);
  assert (R.pop_upalpha r = Error `Eof);

  let reader = Fake_reader.make ~src:"ABc" in
  let r = R.make ~reader in
  assert (R.pop_upalpha r = Ok 'A');
  assert (R.pop_upalpha r = Ok 'B');
  assert (R.peek_upalpha r = Error `Unexpected_char);
  assert (R.pop_upalpha r = Error `Unexpected_char);
  ()

let test_r_loalpha () =
  let reader = Fake_reader.make ~src:"ab" in
  let r = R.make ~reader in
  assert (R.peek_loalpha r = Ok 'a');
  assert (R.pop_loalpha r = Ok 'a');
  assert (R.peek_loalpha r = Ok 'b');
  assert (R.pop_loalpha r = Ok 'b');
  assert (R.peek_loalpha r = Error `Eof);
  assert (R.pop_loalpha r = Error `Eof);

  let reader = Fake_reader.make ~src:"abC" in
  let r = R.make ~reader in
  assert (R.pop_loalpha r = Ok 'a');
  assert (R.pop_loalpha r = Ok 'b');
  assert (R.peek_loalpha r = Error `Unexpected_char);
  assert (R.pop_loalpha r = Error `Unexpected_char);
  ()

let test_r_alpha () =
  let reader = Fake_reader.make ~src:"aB" in
  let r = R.make ~reader in
  assert (R.peek_alpha r = Ok 'a');
  assert (R.pop_alpha r = Ok 'a');
  assert (R.peek_alpha r = Ok 'B');
  assert (R.pop_alpha r = Ok 'B');
  assert (R.peek_alpha r = Error `Eof);
  assert (R.pop_alpha r = Error `Eof);

  let reader = Fake_reader.make ~src:"aB1" in
  let r = R.make ~reader in
  assert (R.pop_alpha r = Ok 'a');
  assert (R.pop_alpha r = Ok 'B');
  assert (R.peek_alpha r = Error `Unexpected_char);
  assert (R.pop_alpha r = Error `Unexpected_char);
  ()

let () =
  let open Alcotest in
  run "uebu"
    [
      ( "r",
        [
          test_case "octet" `Quick test_r_octet;
          test_case "char" `Quick test_r_char;
          test_case "upalpha" `Quick test_r_upalpha;
          test_case "loalpha" `Quick test_r_loalpha;
          test_case "alpha" `Quick test_r_alpha;
        ] );
    ]
