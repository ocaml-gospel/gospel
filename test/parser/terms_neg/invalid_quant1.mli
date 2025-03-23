(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(*@ axiom test : forall . True *)
(* {gospel_expected|
[1] File "invalid_quant1.mli", line 11, characters 24-25:
    11 | (*@ axiom test : forall . True *)
                                 ^
    Error: Syntax error
    
|gospel_expected} *)
