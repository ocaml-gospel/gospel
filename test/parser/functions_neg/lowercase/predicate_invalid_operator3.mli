(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(*@ predicate (+g+) *)
(* {gospel_expected|
[1] File "predicate_invalid_operator3.mli", line 11, characters 16-17:
    11 | (*@ predicate (+g+) *)
                         ^
    Error: Syntax error
    
|gospel_expected} *)
