(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(*@ function f : 'a = 0 *)
(* {gospel_expected|
[1] File "invalid_ret3.mli", line 11, characters 22-23:
    11 | (*@ function f : 'a = 0 *)
                               ^
    Error: Mismatch between type 'a and type integer
    
|gospel_expected} *)
