(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(*@ function f n : integer *)
(* {gospel_expected|
[1] File "function_invalid_parameter2.mli", line 11, characters 15-16:
    11 | (*@ function f n : integer *)
                        ^
    Error: Syntax error
    
|gospel_expected} *)
