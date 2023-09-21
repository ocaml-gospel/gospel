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

(* {gospel_expected|
   [125] File "integer_not_bool1.mli", line 11, characters 14-15:
         11 | (*@ axiom a2: 1 = true *)
                            ^
         Error: This term has type integer but a term was expected of type bool.
   |gospel_expected} *)
