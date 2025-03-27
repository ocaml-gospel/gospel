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

(*@ predicate test (x : ('b, 'a) t) (y : ('a, 'b) t) = x = y *)

(* {gospel_expected|
[1] File "neq_vars6.mli", line 13, characters 59-60:
    13 | (*@ predicate test (x : ('b, 'a) t) (y : ('a, 'b) t) = x = y *)
                                                                    ^
    Error: Mismatch between type ('a, 'b) t
           and type ('b, 'a) t
           Type 'a is incompatible with type 'b
    
|gospel_expected} *)
