(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(*@ predicate f (n1 : integer) (n2 : integer) : prop = 0 *)
(* {gospel_expected|
[1] File "predicate_ret6.mli", line 11, characters 46-47:
    11 | (*@ predicate f (n1 : integer) (n2 : integer) : prop = 0 *)
                                                       ^
    Error: Syntax error
    
|gospel_expected} *)
