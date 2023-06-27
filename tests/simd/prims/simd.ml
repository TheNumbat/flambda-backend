open Stdlib

external int64x2_of_int64s : int64 -> int64 -> int64x2 = "" "int64x2_of_int64s" [@@noalloc] [@@unboxed] 
external int64x2_low_int64 : int64x2 -> int64 = "" "int64x2_low_int64" [@@noalloc] [@@unboxed]
external int64x2_high_int64 : int64x2 -> int64 = "" "int64x2_high_int64" [@@noalloc] [@@unboxed]
let i () = int64x2_of_int64s 1L 2L 

let eq v l h = 
  let v : int64x2 = Obj.magic v in 
  let lv, hv = int64x2_low_int64 v, int64x2_high_int64 v in 
  if l <> lv then Printf.printf "%Ld <> %Ld\n" l lv; 
  if h <> hv then Printf.printf "%Ld <> %Ld\n" h hv
;;

let () = 
  let a : int8x16 = Obj.magic (i ()) in 
  let b : int16x8 = Obj.magic (i ()) in 
  let c : int32x4 = Obj.magic (i ()) in 
  let d : int64x2 = Obj.magic (i ()) in 
  let e : float32x4 = Obj.magic (i ()) in 
  let f : float64x2 = Obj.magic (i ()) in 
  eq (Sys.opaque_identity a) 1L 2L;
  eq (Sys.opaque_identity b) 1L 2L;
  eq (Sys.opaque_identity c) 1L 2L;
  eq (Sys.opaque_identity d) 1L 2L;
  eq (Sys.opaque_identity e) 1L 2L;
  eq (Sys.opaque_identity f) 1L 2L
;;
