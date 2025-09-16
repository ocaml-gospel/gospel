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
(*@ () = f x
    ensures True *)
(* {gospel_expected|
[1] File "invalid_unit1.mli", line 12, characters 4-6:
    12 | (*@ () = f x
             ^^
    Error: This pattern matches on values of type unit, which is incompatible with int
    
|gospel_expected} *)
