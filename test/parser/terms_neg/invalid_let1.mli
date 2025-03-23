(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(*@ axiom test : let x = 0 *)
(* {gospel_expected|
[1] File "invalid_let1.mli", line 11, characters 27-27:
    Error: Syntax error
    
|gospel_expected} *)
