(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(*@ function F = 0 *)
(* {gospel_expected|
[1] File "function_no_ret4.mli", line 11, characters 15-16:
    11 | (*@ function F = 0 *)
                        ^
    Error: Syntax error
    
|gospel_expected} *)
