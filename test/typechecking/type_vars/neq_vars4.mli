(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(*@ type ('a, 'b) t *)

(*@ predicate test (x : ('a, 'a) t) (y : ('b, 'a) t) = x = y *)

(* {gospel_expected|
[1] File "neq_vars4.mli", line 13, characters 59-60:
    13 | (*@ predicate test (x : ('a, 'a) t) (y : ('b, 'a) t) = x = y *)
                                                                    ^
    Error: Mismatch between type ('b, 'a) t
           and type ('a, 'a) t
           Type 'b is incompatible with type 'a
    
|gospel_expected} *)
