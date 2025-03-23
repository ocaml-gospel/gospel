(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(*@ predicate f (X : integer) *)
(* {gospel_expected|
[1] File "predicate_invalid_parameter1.mli", line 11, characters 17-18:
    11 | (*@ predicate f (X : integer) *)
                          ^
    Error: Syntax error
    
|gospel_expected} *)
