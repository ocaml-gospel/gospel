(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(*@ function f (n : integer) n : integer *)
(* {gospel_expected|
[1] File "function_invalid_parameter4.mli", line 11, characters 29-30:
    11 | (*@ function f (n : integer) n : integer *)
                                      ^
    Error: Syntax error
    
|gospel_expected} *)
