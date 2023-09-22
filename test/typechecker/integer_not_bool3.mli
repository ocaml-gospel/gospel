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
(*@ requires x > 0
    variant x = 0
    ensures x
    ensures x > 2
    ensures x > 1 *)

(* {gospel_expected|
   [125] File "integer_not_bool3.mli", line 14, characters 12-13:
         14 |     ensures x
                          ^
         Error: This term has type integer but a term was expected of type bool.
   |gospel_expected} *)
