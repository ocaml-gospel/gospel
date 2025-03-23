(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(*@ function f (X : integer) : integer *)
(* {gospel_expected|
[1] File "function_invalid_parameter1.mli", line 11, characters 16-17:
    11 | (*@ function f (X : integer) : integer *)
                         ^
    Error: Syntax error
    
|gospel_expected} *)
