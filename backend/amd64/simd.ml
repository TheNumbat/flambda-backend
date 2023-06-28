# 2 "backend/amd64/simd.ml"
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

(* SIMD instruction selection for AMD64 *)

open Arch 

let select_operation_sse op = 
  match op with 
  | _ -> None
;;

let select_operation_sse2 op = 
  match select_operation_sse op with 
  | Some instr -> instr 
  | None -> 
  match op with 
  | _ -> None
;;

let select_operation_sse3 op = 
  match select_operation_sse2 op with 
  | Some instr -> instr 
  | None -> 
  if not Arch.sse3_support then None else 
  match op with 
  | _ -> None
;;

let select_operation_ssse3 op = 
  match select_operation_sse3 op with 
  | Some instr -> instr 
  | None -> 
  if not Arch.ssse3_support then None else 
  match op with 
  | _ -> None
;;
let select_operation_sse41 op = 
  match select_operation_ssse3 op with 
  | Some instr -> instr 
  | None -> 
  if not Arch.sse41_support then None else 
  match op with 
  | _ -> None
;;

let select_operation_sse42 op = 
  match select_operation_sse41 op with 
  | Some instr -> instr 
  | None -> 
  if not Arch.sse42_support then None else 
  match op with  
  | "caml_int64_crc_unboxed"
  | "caml_int_crc_untagged" ->
    Some Icrc32q
  | _ -> None
;;

let select_operation = select_operation_sse42
