open Stdlib

(* Technically the C stubs should use __m128i for int vectors, __m128 for float32,
   and __m128d for float64, but all three of these types are just 16-aligned
   16-byte blocks of memory, so the same stubs work for all types. *)

external int64x2_of_int64s : int64 -> int64 -> int64x2 = "" "int64x2_of_int64s" [@@noalloc] [@@unboxed] 
external int64x2_low_int64 : int64x2 -> int64 = "" "int64x2_low_int64" [@@noalloc] [@@unboxed]
external int64x2_high_int64 : int64x2 -> int64 = "" "int64x2_high_int64" [@@noalloc] [@@unboxed]

external int32x4_of_int64s : int64 -> int64 -> int32x4 = "" "int64x2_of_int64s" [@@noalloc] [@@unboxed] 
external int32x4_low_int64 : int32x4 -> int64 = "" "int64x2_low_int64" [@@noalloc] [@@unboxed]
external int32x4_high_int64 : int32x4 -> int64 = "" "int64x2_high_int64" [@@noalloc] [@@unboxed]

external int16x8_of_int64s : int64 -> int64 -> int16x8 = "" "int64x2_of_int64s" [@@noalloc] [@@unboxed] 
external int16x8_low_int64 : int16x8 -> int64 = "" "int64x2_low_int64" [@@noalloc] [@@unboxed]
external int16x8_high_int64 : int16x8 -> int64 = "" "int64x2_high_int64" [@@noalloc] [@@unboxed]

external int8x16_of_int64s : int64 -> int64 -> int8x16 = "" "int64x2_of_int64s" [@@noalloc] [@@unboxed] 
external int8x16_low_int64 : int8x16 -> int64 = "" "int64x2_low_int64" [@@noalloc] [@@unboxed]
external int8x16_high_int64 : int8x16 -> int64 = "" "int64x2_high_int64" [@@noalloc] [@@unboxed]

external float32x4_of_int64s : int64 -> int64 -> float32x4 = "" "int64x2_of_int64s" [@@noalloc] [@@unboxed] 
external float32x4_low_int64 : float32x4 -> int64 = "" "int64x2_low_int64" [@@noalloc] [@@unboxed]
external float32x4_high_int64 : float32x4 -> int64 = "" "int64x2_high_int64" [@@noalloc] [@@unboxed]

external float64x2_of_int64s : int64 -> int64 -> float64x2 = "" "int64x2_of_int64s" [@@noalloc] [@@unboxed] 
external float64x2_low_int64 : float64x2 -> int64 = "" "int64x2_low_int64" [@@noalloc] [@@unboxed]
external float64x2_high_int64 : float64x2 -> int64 = "" "int64x2_high_int64" [@@noalloc] [@@unboxed]

let eq lv hv l h = 
  if l <> lv then Printf.printf "%Ld <> %Ld\n" l lv; 
  if h <> hv then Printf.printf "%Ld <> %Ld\n" h hv
;;

let () = 
  let a : int8x16 = int8x16_of_int64s 1L 2L in
  let b : int16x8 = int16x8_of_int64s 3L 4L in
  let c : int32x4 = int32x4_of_int64s 5L 6L in
  let d : int64x2 = int64x2_of_int64s 7L 8L in
  let e : float32x4 = float32x4_of_int64s 9L 10L in
  let f : float64x2 = float64x2_of_int64s 11L 12L in
  let al, ah = int8x16_low_int64 a, int8x16_high_int64 a in
  eq al ah 1L 2L;
  let bl, bh = int16x8_low_int64 b, int16x8_high_int64 b in
  eq bl bh 3L 4L;
  let cl, ch = int32x4_low_int64 c, int32x4_high_int64 c in
  eq cl ch 5L 6L;
  let dl, dh = int64x2_low_int64 d, int64x2_high_int64 d in
  eq dl dh 7L 8L;
  let el, eh = float32x4_low_int64 e, float32x4_high_int64 e in
  eq el eh 9L 10L;
  let fl, fh = float64x2_low_int64 f, float64x2_high_int64 f in
  eq fl fh 11L 12L
;;
