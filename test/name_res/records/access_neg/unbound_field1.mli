(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(*@ predicate test = forall r. r.x *)

(* {gospel_expected|
[1] File "unbound_field1.mli", line 11, characters 33-34:
    11 | (*@ predicate test = forall r. r.x *)
                                          ^
    Error: Unbound record label x
    
|gospel_expected} *)
