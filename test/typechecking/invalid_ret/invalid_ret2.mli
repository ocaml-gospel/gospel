(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(*@ function f : prop = 0 *)
(* {gospel_expected|
[1] File "invalid_ret2.mli", line 11, characters 24-25:
    11 | (*@ function f : prop = 0 *)
                                 ^
    Error: Mismatch between type prop and type integer
    
|gospel_expected} *)
