(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(*@ function F (n : integer) N : integer *)
(* {gospel_expected|
[1] File "function_invalid_parameter4.mli", line 11, characters 29-30:
    11 | (*@ function F (n : integer) N : integer *)
                                      ^
    Error: Syntax error
    
|gospel_expected} *)
