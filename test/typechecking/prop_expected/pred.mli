(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(*@ predicate test = 0 *)

(* {gospel_expected|
[1] File "pred.mli", line 11, characters 21-22:
    11 | (*@ predicate test = 0 *)
                              ^
    Error: Mismatch between type prop and type integer
    
|gospel_expected} *)
