type t = { y : int; mo : int; d : int; h : int; mi : int; s : int }

let to_string (t : t) =
  Printf.sprintf "%d/%02d/%02d %02d:%02d:%02d" t.y t.mo t.d t.h t.mi t.s

let now () =
  let t = Unix.(time () |> localtime) in
  {
    y = t.tm_year + 1900;
    mo = t.tm_mon + 1;
    d = t.tm_mday;
    h = t.tm_hour;
    mi = t.tm_min;
    s = t.tm_sec;
  }
