(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(*@ predicate (+f) *)
(* {gospel_expected|
[1] File "predicate_invalid_operator2.mli", line 11, characters 16-17:
    11 | (*@ predicate (+f) *)
                         ^
    Error: Syntax error
    
|gospel_expected} *)
