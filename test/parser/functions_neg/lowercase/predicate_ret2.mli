(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(*@ predicate f (n : integer) : prop *)
(* {gospel_expected|
[1] File "predicate_ret2.mli", line 11, characters 30-31:
    11 | (*@ predicate f (n : integer) : prop *)
                                       ^
    Error: Syntax error
    
|gospel_expected} *)
