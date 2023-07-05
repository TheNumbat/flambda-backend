(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 2000 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Arch
open Simd

type arg_loc =
  | Reg
  | Reg_or_amem
  | Reg_or_umem

type ret_loc = Fst

type register_behavior =
  { arg0 : arg_loc;
    arg1 : arg_loc;
    ret : ret_loc
  }

let arg i args = List.nth args i

let extract_constant args name =
  match args with
  | Cmm.Cconst_int (i, _) :: args -> i, args
  | _ ->
    Misc.fatal_errorf "Did not get integer constant as the first argument to %s"
      name

let shuffle_mask x y z w = (x lsl 6) lor (y lsl 4) lor (z lsl 2) lor w

(* TODO: *)
(* implement get and set4 *)
(* add sse2/sse3/ssse3 ops *)
(* tests for every intrinsic *)

let select_operation_sse op args dbg =
  match op with
  | "caml_sse_float32x4_cmp" ->
    let i, args = extract_constant args "caml_sse_float32x4_cmp" in
    Some (Cmp_ps i, args)
  | "caml_sse_float32x4_add" -> Some (Add_ps, args)
  | "caml_sse_float32x4_sub" -> Some (Sub_ps, args)
  | "caml_sse_float32x4_mul" -> Some (Mul_ps, args)
  | "caml_sse_float32x4_div" -> Some (Div_ps, args)
  | "caml_sse_float32x4_max" -> Some (Max_ps, args)
  | "caml_sse_float32x4_min" -> Some (Min_ps, args)
  | "caml_sse_float32x4_rcp" -> Some (Rcp_ps, [arg 0 args; arg 0 args])
  | "caml_sse_float32x4_rsqrt" -> Some (Rsqrt_ps, [arg 0 args; arg 0 args])
  | "caml_sse_float32x4_sqrt" -> Some (Sqrt_ps, [arg 0 args; arg 0 args])
  | "caml_sse_move_high_to_low" -> Some (Move_high_to_low, args)
  | "caml_sse_move_low_to_high" -> Some (Move_low_to_high, args)
  | "caml_sse_interleave_high" -> Some (Interleave_high, args)
  | "caml_sse_interleave_low" -> Some (Interleave_low, args)
  | "caml_sse_shuffle" ->
    let i, args = extract_constant args "caml_sse_shuffle" in
    Some (Shuffle i, args)
    (* CR mslater: (SIMD) these are not single instructions, so should be
       implemented in the vector math library, but we want the ability to
       directly emit a static constant when the arguments are constants. This
       will require moving builtin recognition before the middle-end. *)
  | "caml_sse_float32x4_set1" ->
    Some
      ( Shuffle (shuffle_mask 0 0 0 0),
        [Cmm.Cop (Cscalarcast Float_to_v128_as_32, args, dbg)] )
  (* CR mslater: (SIMD) how do we emit multiple specific instructions mslater:
     overwride emit_expr in selection; change API of this to be able to add
     several instructions *)
  | "caml_sse_float32x4_set4" -> assert false
  | "caml_sse_float32x4_get" ->
    let i, args = extract_constant args "caml_sse_float32x4_get" in
    if i < 0 || i >= 4
    then Misc.fatal_errorf "Bad index (%d) for caml_sse_float32x4_get" i;
    Some (Shuffle (shuffle_mask 0 0 0 i), args)
    (* CR mslater: (SIMD) this also needs a way to generate a Cconst_vec128
       instead of an instruction, or a way to grab an undefined argument of type
       vec128 mslater: this can be moved to builtin detection in cmm_builtins
       mslater: remove xor_ps *)
  | "caml_sse_zero" -> Some (Xor_ps, [])
  | _ -> None

let select_operation_sse2 op args dbg = None

let select_operation_sse3 op args dbg =
  if not !Arch.sse3_support then None else match op with _ -> None

let select_operation_ssse3 op args dbg =
  if not !Arch.ssse3_support then None else match op with _ -> None

let select_operation_sse41 op args dbg =
  if not !Arch.sse41_support then None else match op with _ -> None

let select_operation_sse42 op args dbg =
  if not !Arch.sse42_support
  then None
  else
    match op with
    | "caml_int64_crc_unboxed" | "caml_int_crc_untagged" -> Some (Crc32q, args)
    | _ -> None

let select_simd_instr op args dbg =
  let or_else _try ctr opt =
    match opt with
    | Some x -> Some x
    | None -> Option.map ctr (_try op args dbg)
  in
  None
  |> or_else select_operation_sse (fun (op, args) -> SSE op, args)
  |> or_else select_operation_sse2 (fun (op, args) -> SSE2 op, args)
  |> or_else select_operation_sse3 (fun (op, args) -> SSE3 op, args)
  |> or_else select_operation_ssse3 (fun (op, args) -> SSSE3 op, args)
  |> or_else select_operation_sse41 (fun (op, args) -> SSE41 op, args)
  |> or_else select_operation_sse42 (fun (op, args) -> SSE42 op, args)

let select_operation op args dbg =
  select_simd_instr op args dbg
  |> Option.map (fun (op, args) -> Mach.(Ispecific (Isimd op), args))

let register_behavior op =
  match op with
  | SSE (Cmp_ps _ | Add_ps | Sub_ps | Mul_ps | Div_ps | Max_ps | Min_ps | Xor_ps)
  | SSE (Rcp_ps | Sqrt_ps | Rsqrt_ps)
  | SSE (Interleave_low | Interleave_high | Shuffle _) ->
    { arg0 = Reg; arg1 = Reg_or_amem; ret = Fst }
  | SSE (Move_high_to_low | Move_low_to_high) ->
    { arg0 = Reg; arg1 = Reg; ret = Fst }
  | SSE42 Crc32q -> { arg0 = Reg; arg1 = Reg_or_umem; ret = Fst }
  | _ -> .
