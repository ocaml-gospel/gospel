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

(* ERROR: the term in the ensures clause should be of type integer *)

(* EXPECTED
   [125] File "t7.mli", line 14, characters 12-13:
         Error: This term has type `integer' but a term was expected of type `bool'.
*)
