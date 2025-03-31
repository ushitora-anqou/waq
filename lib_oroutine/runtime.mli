val initialize : (unit -> 'a) -> 'a
val go : (unit -> unit) -> unit
val yield : unit -> unit
val sleep_s : float -> unit
val wait_write : Unix.file_descr -> unit
val wait_read : Unix.file_descr -> unit

val socket :
  ?cloexec:bool ->
  Unix.socket_domain ->
  Unix.socket_type ->
  int ->
  Unix.file_descr

val accept : ?cloexec:bool -> Unix.file_descr -> Unix.file_descr * Unix.sockaddr
val connect : Unix.file_descr -> Unix.sockaddr -> unit
val write : Unix.file_descr -> bytes -> int -> int -> int
val read : Unix.file_descr -> bytes -> int -> int -> int

module Chan : sig
  type 'a t
  type recv_error = [ `Closed ]
  type 'a recv_result = ('a, recv_error) result

  module Select : sig
    type ('a, 'b) spec =
      | Send of ('a t * 'a * (unit -> 'b))
      | Recv of ('a t * ('a recv_result -> 'b))

    type ('ty, 'v, 'b) t =
      | [] : ('v, 'v, 'b) t
      | ( :: ) :
          ('a, 'b) spec * ('ty, 'v, 'b) t
          -> (('a, 'b) spec -> 'ty, 'v, 'b) t
  end

  val make : int -> 'a t
  val close : 'a t -> unit
  val send : 'a -> 'a t -> unit
  val recv : 'a t -> 'a recv_result
  val select : ?default:(unit -> 'b) -> ('ty, 'v, 'b) Select.t -> 'b
end
