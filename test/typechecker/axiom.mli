(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(*@ axiom a1: 1 *)

(* ERROR: axiom should be a formula *)

(* {gospel_expected|
   [125] File "axiom.mli", line 11, characters 14-15:
         11 | (*@ axiom a1: 1 *)
                            ^
         Error: This term has type integer but a term was expected of type bool.
   |gospel_expected} *)
