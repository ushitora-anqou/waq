include Lwt.Infix

let failwithf f = Printf.ksprintf failwith f
let ( |.> ) f g x = f x |> g
let ignore_lwt p = Lwt.map (fun _ -> ()) p

let iota n =
  let rec f acc = function 0 -> acc | n -> f ((n - 1) :: acc) (n - 1) in
  f [] n

let index_by f l =
  let h = Hashtbl.create (List.length l) in
  l |> List.iter (fun x -> Hashtbl.replace h (f x) x);
  h
