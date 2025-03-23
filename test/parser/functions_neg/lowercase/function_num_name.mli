(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(*@ function 1 : integer *)
(* {gospel_expected|
[1] File "function_num_name.mli", line 11, characters 13-14:
    11 | (*@ function 1 : integer *)
                      ^
    Error: Syntax error
    
|gospel_expected} *)
