type t = {
  done_ : unit Runtime.Chan.t;
  parent : t option;
  mtx : Mtx.t;
  mutable children : (remove_from_parent:bool -> unit -> unit) list;
}

let rec propagate_cancel parent cancel =
  let result =
    Mtx.protect parent.mtx (fun () ->
        let closed =
          Runtime.Chan.select
            ~default:(fun () -> false)
            [ Recv (parent.done_, fun _ -> true) ]
        in
        if closed then Error `Closed
        else (
          parent.children <- cancel :: parent.children;
          Ok ()))
  in
  match result with
  | Error `Closed -> cancel ~remove_from_parent:false ()
  | Ok () -> (
      match parent.parent with
      | None -> ()
      | Some parent' -> propagate_cancel parent' cancel)

let done_ ctxt = ctxt.done_

let background () =
  {
    done_ = Runtime.Chan.make 1;
    parent = None;
    children = [];
    mtx = Mtx.create ();
  }

let with_cancel parent f =
  let ctxt = { (background ()) with parent = Some parent } in
  let rec cancel ~remove_from_parent () =
    Mtx.protect ctxt.mtx (fun () ->
        Runtime.Chan.close ctxt.done_;
        ctxt.children
        |> List.iter (fun cancel -> cancel ~remove_from_parent:false ()));
    if remove_from_parent then
      Mtx.protect parent.mtx (fun () ->
          parent.children <- List.filter (( != ) cancel) parent.children)
  in
  propagate_cancel parent cancel;
  let cancel () = cancel ~remove_from_parent:true () in
  Fun.protect ~finally:cancel @@ fun () -> f (ctxt, cancel)
