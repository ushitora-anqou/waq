open Activity

(* Send Undo of Follow to POST inbox *)
let kick (self : Db.Account.t) (acc : Db.Account.t) (f : Db.Follow.t) =
  Job.kick ~name:__FUNCTION__ @@ fun () ->
  Db.Follow.delete ~by:(`uri f.uri);%lwt
  (* Post activity *)
  let obj =
    make_ap_inbox_no_context ~id:f.uri ~typ:"Follow" ~actor:(`String self.uri)
      ~obj:(`String acc.uri)
    |> ap_inbox_no_context_to_yojson
  in
  let body =
    make_ap_inbox ~context
      ~id:(self.uri ^ "#follows" ^/ string_of_int f.id ^/ "undo")
      ~typ:"Undo" ~actor:(`String self.uri) ~obj
    |> ap_inbox_to_yojson
  in
  post_activity_to_inbox ~body ~src:self ~dst:acc
