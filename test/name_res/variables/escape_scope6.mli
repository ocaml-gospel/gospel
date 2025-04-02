(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(*@ axiom test : (exists x. True) /\ x *)

(* {gospel_expected|
[1] File "escape_scope6.mli", line 11, characters 37-38:
    11 | (*@ axiom test : (exists x. True) /\ x *)
                                              ^
    Error: Unbound value x
    
|gospel_expected} *)
