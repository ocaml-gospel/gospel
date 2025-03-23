(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(*@ predicate F (n : integer) n *)
(* {gospel_expected|
[1] File "predicate_invalid_parameter4.mli", line 11, characters 30-31:
    11 | (*@ predicate F (n : integer) n *)
                                       ^
    Error: Syntax error
    
|gospel_expected} *)
