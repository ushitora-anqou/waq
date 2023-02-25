let ( |.> ) f g x = f x |> g
let ignore_lwt (p : _ Lwt.t) = Lwt.bind p (fun _ -> Lwt.return_unit)
let ( |=> ) = Lwt.Infix.( >|= ) (* More intuitive, isn't it? *)

let iota n =
  let rec f acc = function 0 -> acc | n -> f ((n - 1) :: acc) (n - 1) in
  f [] n

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
