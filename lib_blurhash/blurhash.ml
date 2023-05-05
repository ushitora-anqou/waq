let encode_int =
  let characters =
    "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz#$%*+,-.:;=?@[]^_{|}~"
  in
  fun ~value ~length ->
    let buf = Bytes.create length in
    let rec aux x = function
      | 0 -> ()
      | i ->
          let c = String.get characters (x mod 83) in
          Bytes.set buf (i - 1) c;
          aux (x / 83) (i - 1)
    in
    aux value length;
    String.of_bytes buf
