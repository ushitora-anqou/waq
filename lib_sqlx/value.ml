open Util

type t =
  [ `Null
  | `String of string
  | `Int of int
  | `Float of float
  | `Timestamp of Ptime.t
  | `Bool of bool ]
[@@deriving show]

type null_t =
  [ t | `NullString of string option | `NullTimestamp of Ptime.t option ]

let normalize : null_t -> t = function
  | `NullString None -> `Null
  | `NullString (Some s) -> `String s
  | `NullTimestamp None -> `Null
  | `NullTimestamp (Some t) -> `Timestamp t
  | #t as v -> v

let expect_int : t -> int = function
  | `Int i -> i
  | v -> failwithf "Expect int, got: %s" (show v)

let expect_int_opt : t -> int option = function
  | `Null -> None
  | `Int i -> Some i
  | v -> failwithf "Expect int or null, got: %s" (show v)

let expect_string : t -> string = function
  | `String s -> s
  | v -> failwithf "Expect string, got: %s" (show v)

let expect_string_opt : t -> string option = function
  | `Null -> None
  | `String s -> Some s
  | v -> failwithf "Expect string or null, got: %s" (show v)

let expect_timestamp : t -> Ptime.t = function
  | `Timestamp t -> t
  | v -> failwithf "Expect timestamp, got: %s" (show v)

let expect_timestamp_opt : t -> Ptime.t option = function
  | `Null -> None
  | `Timestamp t -> Some t
  | v -> failwithf "Expect timestamp or null, got: %s" (show v)

let expect_bool : t -> bool = function
  | `Bool b -> b
  | v -> failwithf "Expect bool, got: %s" (show v)

let expect_bool_opt : t -> bool option = function
  | `Null -> None
  | `Bool b -> Some b
  | v -> failwithf "Expect bool or null, got: %s" (show v)

let of_int (n : int) = `Int n
let of_string (s : string) = `String s
let of_timestamp (t : Ptime.t) = `Timestamp t

let of_int_opt (i : int option) =
  match i with None -> `Null | Some i -> `Int i

let of_string_opt (s : string option) =
  match s with None -> `Null | Some s -> `String s

let of_timestamp_opt (t : Ptime.t option) =
  match t with None -> `Null | Some t -> `Timestamp t

let of_bool (b : bool) = `Bool b

let of_bool_opt (b : bool option) =
  match b with None -> `Null | Some b -> `Bool b
