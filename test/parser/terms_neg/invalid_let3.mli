(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(*@ axiom test : let x = 0 in in 0 *)
(* {gospel_expected|
[1] File "invalid_let3.mli", line 11, characters 30-32:
    11 | (*@ axiom test : let x = 0 in in 0 *)
                                       ^^
    Error: Syntax error
    
|gospel_expected} *)
