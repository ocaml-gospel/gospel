(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(*@ function f (x : prop) : integer
      variant x *)
(* {gospel_expected|
[1] File "invalid_invariant.mli", line 12, characters 14-15:
    12 |       variant x *)
                       ^
    Error: Mismatch between type prop and type integer
    
|gospel_expected} *)
