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
    ensures x = 2
    ensures x > 2
    ensures x > 1 *)

(* ERROR: the term in the variant clause should be of type integer *)

(* {gospel_expected|
   [125] File "t6.mli", line 13, characters 12-17:
         13 |     variant x = 0
                          ^^^^^
         Error: A term was expected.
   |gospel_expected} *)
