(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(*@ predicate test (x : 'a) (y : 'b) = x = y *)

(* {gospel_expected|
[1] File "neq_vars1.mli", line 11, characters 43-44:
    11 | (*@ predicate test (x : 'a) (y : 'b) = x = y *)
                                                    ^
    Error: Mismatch between type 'b and type 'a
    
|gospel_expected} *)
