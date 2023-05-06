open Lwt.Infix

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
  l |> List.iter (fun x -> Hashtbl.add h (f x) x);
  h

module Lwt_list = struct
  include Lwt_list

  let partition_map_p (f : 'a -> ('b, 'c) Either.t Lwt.t) (l : 'a list) :
      ('b list * 'c list) Lwt.t =
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
  let to_rfc3339 = to_rfc3339 ~tz_offset_s:0
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
  let bind x f = match%lwt x with None -> Lwt.return_none | Some x -> f x
end

module Lwt_io = struct
  include Lwt_io

  let rec write_stream oc s =
    match%lwt Lwt_stream.get s with
    | None -> Lwt.return_unit
    | Some c ->
        Lwt_io.write oc c;%lwt
        write_stream oc s
end

type json_any = Yojson.Safe.t

let yojson_of_json_any : json_any -> Yojson.Safe.t = Fun.id
let json_any_of_yojson : Yojson.Safe.t -> json_any = Fun.id

module Lwt_unix = struct
  include Lwt_unix

  let rec mkpath path mode =
    match path with
    | "." | "/" -> Lwt.return_unit
    | _ -> (
        mkpath (Filename.dirname path) mode;%lwt
        match%lwt stat path >|= fun x -> x.st_kind with
        | S_DIR -> Lwt.return_unit
        | _ -> failwith "File already exists"
        | exception Unix.Unix_error (ENOENT, "stat", _) -> mkdir path mode)
end

let int_to_3digits i =
  let s = Printf.sprintf "%012d" i in
  String.[ sub s 0 3; sub s 3 3; sub s 6 3; sub s 9 3 ]

let blurhash_file ~x_components ~y_components path =
  let src =
    match OImages.(load path [] |> tag) with
    | Rgb24 img -> img
    | Rgba32 img -> img#to_rgb24
    | Index8 img -> img#to_rgb24
    | Index16 img -> img#to_rgb24
    | Cmyk32 _ -> failwith "Not supported image type: Cmyk32"
  in
  Blurhash.blur_hash_for_pixels ~x_components ~y_components ~width:src#width
    ~height:src#height ~rgb:(Bytes.to_string src#dump)
    ~bytes_per_row:(src#width * 3)
