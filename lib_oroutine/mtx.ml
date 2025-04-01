type t = { ch : unit Runtime.Chan.t }

let create () = { ch = Runtime.Chan.make 1 }
let lock mtx = Runtime.Chan.send () mtx.ch

let try_lock mtx =
  Runtime.Chan.select ~default:(Fun.const false)
    [ Send (mtx.ch, (), Fun.const true) ]

let unlock mtx =
  Runtime.Chan.select
    ~default:(fun () -> failwith "Mtx.unlock: not locked")
    [ Recv (mtx.ch, Fun.const ()) ]

let protect mtx f =
  lock mtx;
  Fun.protect ~finally:(fun () -> unlock mtx) f
