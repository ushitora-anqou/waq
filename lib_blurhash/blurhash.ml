(**
  FIXME: This implementation is written by naively translating Blurhash's
  original C code into OCaml, so it's not very sophisticated nor performant.
  We can do much better by using e.g., Bigarray.
*)

let iota n =
  let rec f acc = function 0 -> acc | n -> f ((n - 1) :: acc) (n - 1) in
  f [] n

let sign_pow v e = if v < 0.0 then -.(Float.pow (-.v) e) else Float.pow v e

let linear_to_srgb (value : float) : int =
  let v = max 0.0 (min 1.0 value) in
  (if v < 0.0031308 then (v *. 12.92 *. 255.0) +. 0.5
   else (((1.055 *. Float.pow v (1.0 /. 2.4)) -. 0.055) *. 255.0) +. 0.5)
  |> int_of_float

let srgb_to_linear (value : int) : float =
  let v = float_of_int value /. 255.0 in
  if v <= 0.04045 then v /. 12.92 else Float.pow ((v +. 0.055) /. 1.055) 2.4

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

let encode_ac ~r ~g ~b ~max_value =
  let quantize x =
    max 0.0 (min 18.0 (floor ((sign_pow (x /. max_value) 0.5 *. 9.0) +. 9.5)))
    |> int_of_float
  in
  let quant_r = quantize r in
  let quant_g = quantize g in
  let quant_b = quantize b in
  (quant_r * 19 * 19) + (quant_g * 19) + quant_b

let encode_dc ~r ~g ~b =
  let rounded_r = linear_to_srgb r in
  let rounded_g = linear_to_srgb g in
  let rounded_b = linear_to_srgb b in
  (rounded_r lsl 16) + (rounded_g lsl 8) + rounded_b

let multiply_basis_function ~x_component ~y_component ~width ~height ~rgb
    ~bytes_per_row =
  let x_component_f = float_of_int x_component in
  let y_component_f = float_of_int y_component in
  let width_f = float_of_int width in
  let height_f = float_of_int height in

  let normalization = if x_component = 0 && y_component = 0 then 1.0 else 2.0 in

  let rec aux (r, g, b) i =
    if i >= width * height then (r, g, b)
    else
      let x = i mod width in
      let y = i / width in
      let basis =
        Float.(
          cos (pi *. x_component_f *. float_of_int x /. width_f)
          *. cos (pi *. y_component_f *. float_of_int y /. height_f))
      in
      let f v i =
        v
        +. basis
           *. srgb_to_linear
                (String.get_uint8 rgb ((3 * x) + i + (y * bytes_per_row)))
      in
      aux (f r 0, f g 1, f b 2) (i + 1)
  in
  let r, g, b = aux (0.0, 0.0, 0.0) 0 in

  let scale = normalization /. (width_f *. height_f) in
  (r *. scale, g *. scale, b *. scale)

let blur_hash_for_pixels ~x_components ~y_components ~width ~height ~rgb
    ~bytes_per_row =
  if x_components < 1 || x_components > 9 then failwith "Invalid x_components";
  if y_components < 1 || y_components > 9 then failwith "Invalid y_components";

  let factors =
    iota (x_components * y_components)
    |> List.map (fun i ->
           let x_component = i mod x_components in
           let y_component = i / x_components in
           multiply_basis_function ~x_component ~y_component ~width ~height ~rgb
             ~bytes_per_row)
  in

  let dc_r, dc_g, dc_b = List.hd factors in
  let ac = List.tl factors in
  let ac_count = (x_components * y_components) - 1 in

  let buf = Buffer.create 30 in

  let size_flag = x_components - 1 + ((y_components - 1) * 9) in
  Buffer.add_string buf (encode_int ~value:size_flag ~length:1);

  let max_value =
    if ac_count > 0 then (
      let actual_max_value =
        ac
        |> List.map (fun (r, g, b) -> [ r; g; b ])
        |> List.flatten |> List.map Float.abs |> List.fold_left max 0.0
      in
      let quantized_max_value =
        max 0 (min 82 (int_of_float ((actual_max_value *. 166.0) -. 0.5)))
      in
      let max_value = (float_of_int quantized_max_value +. 1.0) /. 166.0 in
      Buffer.add_string buf (encode_int ~value:quantized_max_value ~length:1);
      max_value)
    else (
      Buffer.add_string buf (encode_int ~value:0 ~length:1);
      1.0)
  in

  Buffer.add_string buf
    (encode_int ~value:(encode_dc ~r:dc_r ~g:dc_g ~b:dc_b) ~length:4);
  ac
  |> List.iter (fun (r, g, b) ->
         Buffer.add_string buf
           (encode_int ~value:(encode_ac ~r ~g ~b ~max_value) ~length:2));

  Buffer.contents buf
