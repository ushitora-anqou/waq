let ( |.> ) f g x = g (f x)
let ( @.@ ) f g x = f (g x)
let ignore_lwt (p : _ Lwt.t) = Lwt.bind p (fun _ -> Lwt.return_unit)

(* Thanks to: https://github.com/camlspotter/ocaml-zippy-tutorial-in-japanese/blob/master/langspec.md#functional-composition *)
let ( *> ) f g x = g (f x)
let ( *< ) f g x = f (g x)

(* Thanks to: https://github.com/camlspotter/ocaml-zippy-tutorial-in-japanese/blob/master/langspec.md#haskells- *)
external ( & ) : ('a -> 'b) -> 'a -> 'b = "%apply"

let iota n =
  let rec f acc = function 0 -> acc | n -> f ((n - 1) :: acc) (n - 1) in
  f [] n

let index_by f l =
  let h = Hashtbl.create (List.length l) in
  l |> List.iter (fun x -> Hashtbl.replace h (f x) x);
  h

module Lwt_list = struct
  include Lwt_list

  let partition_map_p (f : 'a -> ('b, 'c) Either.t Lwt.t) (l : 'a list) :
      ('b list * 'c list) Lwt.t =
    let open Lwt.Infix in
    l |> List.map f
    |> List.fold_left
         (fun a p ->
           let%lwt left, right = a in
           p >|= function
           | Either.Left x -> (x :: left, right)
           | Right y -> (left, y :: right))
         (Lwt.return ([], []))
    >|= fun (left, right) -> (List.rev left, List.rev right)
end

module Ptime = struct
  include Ptime

  let now () = Unix.gettimeofday () |> of_float_s |> Option.get
  let to_int : t -> int = to_span |.> Span.to_int_s |.> Option.get
end

let acct (username : string) (domain : string option) : string =
  match domain with None -> username | Some domain -> username ^ "@" ^ domain

let ( ^/ ) s1 s2 = s1 ^ "/" ^ s2

module List = struct
  include List

  let singleton x = [ x ]

  let take n l =
    let rec aux acc = function
      | 0, _ -> List.rev acc
      | n, x :: xs -> aux (x :: acc) (n - 1, xs)
      | _ -> failwith "take: not enough elements"
    in
    aux [] (n, l)
end

module Lwt_option = struct
  let map f = function
    | None -> Lwt.return_none
    | Some v -> Lwt.map Option.some (f v)

  let iter f = function None -> Lwt.return_unit | Some v -> f v
end

type json_any = Yojson.Safe.t

let yojson_of_json_any : json_any -> Yojson.Safe.t = Fun.id
let json_any_of_yojson : Yojson.Safe.t -> json_any = Fun.id
