(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

val f : int -> int
(*@ x = f ()
    ensures True *)

(* {gospel_expected|
[1] File "invalid_unit2.mli", line 12, characters 10-12:
    12 | (*@ x = f ()
                   ^^
    Error: This pattern matches on values of type unit, which is incompatible with int
    
|gospel_expected} *)
