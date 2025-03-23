(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(*@ predicate F (n : integer) (X : integer) *)
(* {gospel_expected|
[1] File "predicate_invalid_parameter3.mli", line 11, characters 31-32:
    11 | (*@ predicate F (n : integer) (X : integer) *)
                                        ^
    Error: Syntax error
    
|gospel_expected} *)
