(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(*@ function f (x : integer) : integer
      requires x *)
(* {gospel_expected|
[1] File "invalid_pre.mli", line 12, characters 15-16:
    12 |       requires x *)
                        ^
    Error: Mismatch between type integer and type prop
    
|gospel_expected} *)
