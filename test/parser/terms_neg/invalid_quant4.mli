(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(*@ axiom test : exists x : . x*)
(* {gospel_expected|
[1] File "invalid_quant4.mli", line 11, characters 28-29:
    11 | (*@ axiom test : exists x : . x*)
                                     ^
    Error: Syntax error
    
|gospel_expected} *)
