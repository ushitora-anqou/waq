type single_pattern = L of string | P of string | S
type t = single_pattern list

let split_on_slash s =
  s |> String.split_on_char '/' |> List.tl |> List.filter (( <> ) "")

let of_string (src : string) : t =
  src |> split_on_slash
  |> List.map (function
       | "*" -> S
       | x when String.starts_with ~prefix:":" x -> P x
       | x -> L x)

let perform ~(pat : t) (src : string) : (string * string) list option =
  let rec aux param = function
    | [], [] | _, [ S ] -> Some param
    | x :: xs, L y :: ys when x = y -> aux param (xs, ys)
    | x :: xs, P y :: ys -> aux ((y, x) :: param) (xs, ys)
    | _ -> None
  in
  aux [] (split_on_slash src, pat)
