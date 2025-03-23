(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(*@ predicate F (n1 : integer) (n2 : integer) : prop *)
(* {gospel_expected|
[1] File "predicate_ret3.mli", line 11, characters 46-47:
    11 | (*@ predicate F (n1 : integer) (n2 : integer) : prop *)
                                                       ^
    Error: Syntax error
    
|gospel_expected} *)
