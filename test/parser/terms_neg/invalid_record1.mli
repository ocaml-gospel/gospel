(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(*@ type r = { x : integer } *)

(*@ axiom test : { x = 0  *)
(* {gospel_expected|
[1] File "invalid_record1.mli", line 13, characters 26-26:
    Error: Syntax error
    
|gospel_expected} *)
