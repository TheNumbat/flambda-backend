(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2017--2019 OCamlPro SAS                                    *)
(*   Copyright 2017--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** SIMD vector numeric type layouts. *)

module Vec128 : sig
  type t =
    | Int8x16
    | Int16x8
    | Int32x4
    | Int64x2
    | Float32x4
    | Float64x2

  val name : t -> string

  val name_lowercase : t -> string

  val equal : t -> t -> bool

  val to_lambda : t -> Lambda.vec128_type

  val of_lambda : Lambda.vec128_type -> t

  module Bit_pattern : sig
    (** 128-bit value whose comparison and equality relations are lexicographically
        ordered by bit pattern. *)

    include Container_types.S

    val zero : t

    val to_int64s : t -> int64 * int64

    val of_int64s : int64 * int64 -> t
  end
end

type t = Vec128 of Vec128.t

val is_vec128 : t -> bool

val name : t -> string

val name_lowercase : t -> string

val equal : t -> t -> bool

val to_lambda : t -> Lambda.boxed_vector

val of_lambda : Lambda.boxed_vector -> t
