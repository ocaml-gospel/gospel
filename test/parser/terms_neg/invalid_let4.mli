(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(*@ axiom test : let let x = 0 in x *)
(* {gospel_expected|
[1] File "invalid_let4.mli", line 11, characters 21-24:
    11 | (*@ axiom test : let let x = 0 in x *)
                              ^^^
    Error: Syntax error
    
|gospel_expected} *)
