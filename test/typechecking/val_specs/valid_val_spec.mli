(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

val n : int
(*@ ensures n = 0 *)

type t
(*@ model : integer sequence *)

val x : t
(*@ ensures x = Sequence.empty *)
(* {gospel_expected|
[1] File "valid_val_spec.mli", line 12, characters 4-11:
    12 | (*@ ensures n = 0 *)
             ^^^^^^^
    Error: Syntax error

|gospel_expected} *)
