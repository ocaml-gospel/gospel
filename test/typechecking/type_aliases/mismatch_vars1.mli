(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(*@ type 'a t1 *)
(*@ type 'a t2 = 'a t1 *)

(*@ predicate test (x : 'a t1) (y : 'b t2) = x = y *)

(* {gospel_expected|
[1] File "mismatch_vars1.mli", line 14, characters 49-50:
    14 | (*@ predicate test (x : 'a t1) (y : 'b t2) = x = y *)
                                                          ^
    Error: Mismatch between type 'b t1
           and type 'a t1
           Type 'b is incompatible with type 'a
    
|gospel_expected} *)
