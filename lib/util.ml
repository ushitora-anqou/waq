let ( |.> ) f g x = f x |> g
let ignore_lwt (p : _ Lwt.t) = Lwt.bind p (fun _ -> Lwt.return_unit)
let ( |=> ) = Lwt.Infix.( >|= ) (* More intuitive, isn't it? *)
let regex ptn = Re.(Pcre.re ptn |> compile)

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

  let to_http_date (v : t) : string =
    let string_of_week = function
      | `Sun -> "Sun"
      | `Mon -> "Mon"
      | `Tue -> "Tue"
      | `Wed -> "Wed"
      | `Thu -> "Thu"
      | `Fri -> "Fri"
      | `Sat -> "Sat"
    in
    let string_of_month = function
      | 1 -> "Jan"
      | 2 -> "Feb"
      | 3 -> "Mar"
      | 4 -> "Apr"
      | 5 -> "May"
      | 6 -> "Jun"
      | 7 -> "Jul"
      | 8 -> "Aug"
      | 9 -> "Sep"
      | 10 -> "Oct"
      | 11 -> "Nov"
      | 12 -> "Dec"
      | _ -> assert false
    in
    let (year, month, day_of_month), ((hour, minute, second), _) =
      to_date_time v
    in
    let month = string_of_month month in
    let day_name = weekday v |> string_of_week in
    Printf.sprintf "%s, %02d %s %d %02d:%02d:%02d GMT" day_name day_of_month
      month year hour minute second

  let to_int : t -> int = to_span |.> Span.to_int_s |.> Option.get
end
