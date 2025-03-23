(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(*@ axiom test : let X = 0 in X *)
(* {gospel_expected|
[1] File "invalid_let2.mli", line 11, characters 21-22:
    11 | (*@ axiom test : let X = 0 in X *)
                              ^
    Error: Syntax error
    
|gospel_expected} *)
