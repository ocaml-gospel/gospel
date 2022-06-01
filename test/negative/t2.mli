(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(*@ axiom a2: 1 = true *)

(* ERROR: type mismatch bool and integer *)

(* {gospel_expected|
   [125] File "t2.mli", line 11, characters 11-12:
         Error: This term has type `integer' but a term was expected of type `bool'.
   |gospel_expected} *)
