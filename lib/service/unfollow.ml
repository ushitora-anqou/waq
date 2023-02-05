open Util
open Activity

(* Send Undo of Follow to POST inbox *)
let kick (self : Db.Account.t) (acc : Db.Account.t) (f : Db.Follow.t) =
  Job.kick ~name:__FUNCTION__ @@ fun () ->
  Db.Follow.delete ~uri:f.uri ();%lwt
  (* Post activity *)
  let obj = make_follow ~id:f.uri ~actor:self.uri ~obj:acc.uri |> of_follow in
  let body =
    make_undo
      ~id:(self.uri ^ "#follows" ^/ string_of_int f.id ^/ "undo")
      ~actor:(`String self.uri) ~obj
    |> of_undo |> to_yojson
  in
  post_activity_to_inbox ~body ~src:self ~dst:acc
