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

(* EXPECTED
   [125] File "t5.mli", line 12, characters 10-11:
         Error: This term has type `integer' but a term was expected of type `bool'.
*)
