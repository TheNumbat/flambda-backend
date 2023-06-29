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

(* SIMD instructions for AMD64 *)

open Format

type sse_operation = |

type sse2_operation = |

type sse3_operation = |

type ssse3_operation = |

type sse41_operation = |

type sse42_operation = Crc32q

type operation =
  | SSE of sse_operation
  | SSE2 of sse2_operation
  | SSE3 of sse3_operation
  | SSSE3 of ssse3_operation
  | SSE41 of sse41_operation
  | SSE42 of sse42_operation

let equal_operation_sse42 l r =
  match (l : sse42_operation), (r : sse42_operation) with
  | Crc32q, Crc32q -> true

let equal_operation l r =
  match l, r with SSE42 l, SSE42 r -> equal_operation_sse42 l r | _ -> .

let print_operation_sse42 printreg op ppf arg =
  match op with
  | Crc32q -> fprintf ppf "crc32 %a %a" printreg arg.(0) printreg arg.(1)

let print_operation printreg op ppf arg =
  match op with SSE42 op -> print_operation_sse42 printreg op ppf arg | _ -> .
