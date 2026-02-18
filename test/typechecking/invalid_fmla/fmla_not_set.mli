(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(*@ axiom test : { v | True } *)

(* {gospel_expected|
[1] File "fmla_not_set.mli", line 11, characters 17-29:
    11 | (*@ axiom test : { v | True } *)
                          ^^^^^^^^^^^^
    Error: Mismatch between type prop and type 'a set
    
|gospel_expected} *)
