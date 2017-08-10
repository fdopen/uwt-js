type not_specified
type t = not_specified Js.t

module JU = Js.Unsafe

let _Buffer = JU.global##._Buffer
let process = JU.global##.process
let bbinding = process##binding (Js.string "buffer")

let create (n:int) : t =
  if n < 0 then
    invalid_arg "Bigarray.create: negative dimension";
  if n > bbinding##.kMaxLength then
    invalid_arg "Cbytes.create: too large";
  let b = _Buffer##allocUnsafe n in
  b

let length (b:t) : int = (JU.coerce b)##.length

external unsafe_get : t -> int -> char = "caml_js_get"
external unsafe_set : t -> int -> char -> unit = "caml_js_set"

let get b i =
  if i < 0 || i >= length b then
    invalid_arg "index out of bounds";
  unsafe_get b i

let set b i c =
  if i < 0 || i >= length b then
    invalid_arg "index out of bounds";
  unsafe_set b i c

let unsafe_fill (bytes:t) ofs len (ch:char) =
  let _ = bbinding##fill bytes ch ofs (ofs + len) (Js.string "latin1") in
  ()

let fill bytes ofs len ch =
  if ofs < 0 || len < 0 || ofs > length bytes - len then
    invalid_arg "Cbytes.fill"
  else
    unsafe_fill bytes ofs len ch

let unsafe_blit_from_bytes b b_pos ar ar_pos len =
  let real_b = Obj.magic b in
  let bcont = real_b##.c in
  if real_b##.t = 4 then (* array case *)
    (* copying between Buffers and MlStrings will only work, if the arrays
       are uint8array - which should be the case *)
    let () = bbinding##copy bcont ar ar_pos b_pos (b_pos + len) in ()
  else
  let bcont_last = bcont##.length - 1 in
  let end' = b_pos + len - 1 in
  let bcont_end = min end' bcont_last in
  let j = ref ar_pos in
  for i = b_pos to bcont_end do
    let c = bcont##charCodeAt i in
    unsafe_set ar !j c;
    incr j;
  done;
  if end' > bcont_end then
    unsafe_fill ar !j (end' - bcont_end) '\000'

let unsafe_blit_from_string b b_pos ar ar_pos len =
  unsafe_blit_from_bytes (Obj.magic b) b_pos ar ar_pos len

let blit_from_bytes src_buf src_ofs dst_buf dst_ofs len =
  if (len < 0
      || src_ofs < 0 || src_ofs > Bytes.length src_buf - len
      || dst_ofs < 0 || dst_ofs > length dst_buf - len) then
    invalid_arg "Cbytes.blit_from_bytes"
  else
    unsafe_blit_from_bytes src_buf src_ofs dst_buf dst_ofs len

let convert_bytes_to_array (x:Bytes.t) : unit =
  let rx = Obj.magic x in
  if rx##.t <> 4 then
    JU.fun_call (JU.js_expr "caml_convert_string_to_array") [|JU.inject x|];
  ()

let unsafe_blit_to_bytes (src_buf:t) src_ofs (dst_buf:Bytes.t) dst_ofs len =
  convert_bytes_to_array dst_buf;
  let real_dst = Obj.magic dst_buf in
  let _ = bbinding##copy src_buf real_dst##.c dst_ofs src_ofs (len + src_ofs) in
  ()

let blit_to_bytes (src_buf:t) src_ofs (dst_buf:Bytes.t) dst_ofs len =
  if (len < 0
      || src_ofs < 0 || src_ofs > length src_buf - len
      || dst_ofs < 0 || dst_ofs > Bytes.length dst_buf - len) then
    invalid_arg "Cbytes.blit_to_bytes"
  else
    unsafe_blit_to_bytes src_buf src_ofs dst_buf dst_ofs len

let unsafe_blit (src_buf:t) src_ofs (dst_buf:t) (dst_ofs:int) len : unit =
  let _ = bbinding##copy src_buf dst_buf dst_ofs src_ofs (src_ofs + len) in
  ()

let blit src_buf src_ofs dst_buf dst_ofs len =
  if (len < 0
      || src_ofs < 0 || src_ofs > length src_buf - len
      || dst_ofs < 0 || dst_ofs > length dst_buf - len) then
    invalid_arg "Cbytes.blit"
  else
    unsafe_blit src_buf src_ofs dst_buf dst_ofs len

let proxy t ofs len : t =
  let real_l = length t in
  if len < 0 || ofs < 0 || ofs > real_l - len then
    invalid_arg "Cbytes.proxy" ;
  if ofs = 0 && len = real_l then t
  else (JU.coerce t)##slice ofs (ofs + len)

let extract t ofs len : t =
  if len < 0 || ofs < 0 || ofs > length t - len then
    invalid_arg "Cbytes.extract";
  let nt = create len in
  unsafe_blit t ofs nt 0 len;
  nt

let copy t =
  let len = length t in
  let nt = create len in
  unsafe_blit t 0 nt 0 len;
  nt

let of_string s =
  let len = String.length s in
  let nt = create len in
  unsafe_blit_from_string s 0 nt 0 len;
  nt

let of_bytes (b:Bytes.t) = of_string (Obj.magic b)

let to_bytes (t:t) =
  let c = JU.new_obj JU.global##._Uint8Array [| JU.inject t |] in
  let constr = JU.js_expr "MlString" in
  let res = new%js constr 4 c (length t) in
  Obj.magic res

let to_string = to_bytes

let of_string_maybe_proxy ~pos ~len buf =
  let real_b = Obj.magic buf in
  if real_b##.t = 4 then
    if pos = 0 && len = real_b##.l then
      real_b##.c
    else
      JU.new_obj
        JU.global##._Uint8Array
        [| real_b##.c##.buffer ;
           JU.inject (real_b##.c##.byteOffset + pos);
           JU.inject len |]
  else
  let ar = create len in
  unsafe_blit_from_string buf pos ar 0 len;
  ar

let of_bytes_maybe_proxy ~pos ~len buf =
  of_string_maybe_proxy ~pos ~len (Obj.magic buf)

(* TODO: Under which condition is this faster than a simple loop? *)
let unsafe_memchr ~pos ~len buf needle =
  let buf_len = length buf
  and b = Obj.magic buf in
  let x =
    if pos = 0 && len = buf_len then b else
      JU.new_obj
        JU.global##._Uint8Array
        [| b##.buffer ; JU.inject (b##.byteOffset + pos) ; JU.inject len |] in
  let c = bbinding##indexOfNumber x (Char.code needle) 0 Js._true in
  if c < 0 then -1                      (* 0: offset, Js._true: forward search*)
  else c + pos

let to_bigarray (t:t) : (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t =
  let dims = new%js Js.array_length 1 in
  JU.set dims 0 (length t);
  JU.fun_call (JU.js_expr "caml_ba_create_from")
    [|
      JU.inject t ; (* data *)
      JU.inject Js.null; (*data2 *)
      JU.inject 0 ; (* data_type *)
      JU.inject 12; (* kind *)
      JU.inject 0 ; (* layout *)
      JU.inject dims
    |]

let from_bigarray (ar:((char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t)) : t =
  let buf = (Obj.magic ar)##.data in
  _Buffer##from buf##.buffer buf##.byteOffset buf##.byteLength
