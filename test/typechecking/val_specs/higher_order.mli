(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

type t
(*@ model size : integer
    model contents : integer sequence *)

val init : int -> (int -> int) -> t
(*@ t = init n f
    ensures t.size = n
    ensures t.contents = Sequence.init n f *)
