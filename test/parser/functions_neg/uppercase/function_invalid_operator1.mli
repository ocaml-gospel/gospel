(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(*@ function (F) : integer*)
(* {gospel_expected|
[1] File "function_invalid_operator1.mli", line 11, characters 14-15:
    11 | (*@ function (F) : integer*)
                       ^
    Error: Syntax error
    
|gospel_expected} *)
