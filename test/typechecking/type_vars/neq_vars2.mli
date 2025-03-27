(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(*@ predicate test (x : 'a sequence) (y : 'b sequence) = x = y *)

(* {gospel_expected|
[1] File "neq_vars2.mli", line 11, characters 61-62:
    11 | (*@ predicate test (x : 'a sequence) (y : 'b sequence) = x = y *)
                                                                      ^
    Error: Mismatch between type 'b sequence
           and type 'a sequence
           Type 'b is incompatible with type 'a
    
|gospel_expected} *)
