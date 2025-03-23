(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(*@ function F (n1 : integer) (n2 : integer) = 0 *)
(* {gospel_expected|
[1] File "function_no_ret6.mli", line 11, characters 45-46:
    11 | (*@ function F (n1 : integer) (n2 : integer) = 0 *)
                                                      ^
    Error: Syntax error
    
|gospel_expected} *)
