(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(*@ type 'a t = 'a *)

(*@ predicate test (x : 'a t) (y : 'b) = x = y *)

(* {gospel_expected|
[1] File "mismatch_vars2.mli", line 13, characters 45-46:
    13 | (*@ predicate test (x : 'a t) (y : 'b) = x = y *)
                                                      ^
    Error: Mismatch between type 'b and type 'a
    
|gospel_expected} *)
