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

module ImageMagick = struct
  let convert_exe = "/usr/bin/convert"

  let convert ~(input_type : [ `PNG | `JPEG ])
      ~(input_data : string Lwt_stream.t) ~(output_file_name : string) =
    Lwt_io.with_temp_file
      ~suffix:(match input_type with `PNG -> ".png" | `JPEG -> ".jpeg")
    @@ fun (temp_file_name, oc) ->
    Lwt_io.write_stream oc input_data;%lwt
    Lwt_io.flush oc;%lwt
    let com =
      ( convert_exe,
        [| convert_exe; temp_file_name; "-strip"; output_file_name |] )
    in
    Lwt_process.exec com >|= function
    | WEXITED 0 -> ()
    | _ -> failwith "ImageMagick.convert: failed to convert"
end
