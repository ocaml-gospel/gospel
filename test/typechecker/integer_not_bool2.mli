(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(*@ function p (x:integer):integer = x *)
(*@ requires x
    variant x
    ensures x = 2
    ensures x > 2
    ensures x > 1 *)

(* ERROR: the term in the requires clause should be of type bool or prop *)

(* {gospel_expected|
   [125] File "integer_not_bool2.mli", line 12, characters 13-14:
         12 | (*@ requires x
                           ^
         Error: This term has type integer but a term was expected of type bool.
   |gospel_expected} *)
