(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(*@ function f : integer = True *)
(* {gospel_expected|
[1] File "invalid_ret1.mli", line 11, characters 27-31:
    11 | (*@ function f : integer = True *)
                                    ^^^^
    Error: Mismatch between type integer and type prop
    
|gospel_expected} *)
