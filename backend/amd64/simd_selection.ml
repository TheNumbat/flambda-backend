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

type register_behavior = Two_arg_instr

let select_operation_sse _op = None

let select_operation_sse2 _op = None

let select_operation_sse3 op =
  if not !Arch.sse3_support then None else match op with _ -> None

let select_operation_ssse3 op =
  if not !Arch.ssse3_support then None else match op with _ -> None

let select_operation_sse41 op =
  if not !Arch.sse41_support then None else match op with _ -> None

let select_operation_sse42 op =
  if not !Arch.sse42_support
  then None
  else
    match op with
    | "caml_int64_crc_unboxed" | "caml_int_crc_untagged" -> Some Crc32q
    | _ -> None

let select_operation op =
  let or_else _try ctr opt =
    match opt with Some x -> Some x | None -> Option.map ctr (_try op)
  in
  None
  |> or_else select_operation_sse (fun op -> SSE op)
  |> or_else select_operation_sse2 (fun op -> SSE2 op)
  |> or_else select_operation_sse3 (fun op -> SSE3 op)
  |> or_else select_operation_ssse3 (fun op -> SSSE3 op)
  |> or_else select_operation_sse41 (fun op -> SSE41 op)
  |> or_else select_operation_sse42 (fun op -> SSE42 op)

let register_behavior op =
  match op with SSE42 Crc32q -> Two_arg_instr | _ -> .
