(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(*@ type 'a t1 = 'a sequence *)

(*@ predicate test (x : 'a t1) (y : integer t1) = x = y *)

(* {gospel_expected|
[1] File "mismatch_vars3.mli", line 13, characters 54-55:
    13 | (*@ predicate test (x : 'a t1) (y : integer t1) = x = y *)
                                                               ^
    Error: Mismatch between type integer sequence
           and type 'a sequence
           Type integer is incompatible with type 'a
    
|gospel_expected} *)
