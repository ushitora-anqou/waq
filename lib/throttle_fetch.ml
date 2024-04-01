(*
module Throttle = struct
  type t = {
    rate : int;
    mtx : Eio.Mutex.t;
    count : (string, int) Hashtbl.t;
    mutable cleaning : bool;
  }

  let create ~rate ~n =
    {
      rate;
      mtx = Eio.Mutex.create ();
      count = Hashtbl.create n;
      cleaning = false;
    }

  let clean clock t =
    Eio.Time.sleep clock 1.0;
    Eio.Mutex.use_rw ~protect:true t.mtx (fun () ->
        Hashtbl.clear t.count;
        t.cleaning <- false)

  let wait ~sw clock t k =
    Eio.Mutex.use_rw ~protect:true t.mtx (fun () ->
        if not t.cleaning then (
          t.cleaning <- true;
          Eio.Fiber.fork ~sw (fun () -> clean clock t));

        match Hashtbl.find_opt t.count k with
        | None ->
            Hashtbl.add t.count k 1;
            true
        | Some c when c > t.rate -> false
        | Some c ->
            Hashtbl.replace t.count k (c + 1);
            true)
end

module StringHash = struct
  type t = string

  let equal = ( = )
  let hash = Hashtbl.hash
end
*)

module Throttle = struct
  type t = { semaphore : Eio.Semaphore.t }

  let create ~rate = { semaphore = Eio.Semaphore.make rate }

  let wait { semaphore } _k f =
    Eio.Semaphore.acquire semaphore;
    Fun.protect ~finally:(fun () -> Eio.Semaphore.release semaphore) f
end

let limitter = Throttle.create ~rate:10

let call f url =
  if Config.debug_no_throttle_fetch () then f url
  else
    let host = Uri.(of_string url |> host) |> Option.value ~default:"" in
    Throttle.wait limitter host (fun () -> f url)

let f env ?(headers = []) ?(meth = `GET) ?(body = "") ?(sign = None) =
  call (Yume.Client.fetch env ~headers ~meth ~body ~sign)

let f_exn env ?(headers = []) ?(meth = `GET) ?(body = "") ?(sign = None) =
  call (Yume.Client.fetch_exn env ~headers ~meth ~body ~sign)

let http_get url = f_exn url
