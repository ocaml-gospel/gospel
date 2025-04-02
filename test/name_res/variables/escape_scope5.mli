(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(*@ axiom ax : (forall x. True) /\ x *)

(* {gospel_expected|
[1] File "escape_scope5.mli", line 11, characters 35-36:
    11 | (*@ axiom ax : (forall x. True) /\ x *)
                                            ^
    Error: Unbound value x
    
|gospel_expected} *)
