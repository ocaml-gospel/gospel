(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(*@ predicate p = p *)

(* {gospel_expected|
[1] File "non_recursive2.mli", line 11, characters 18-19:
    11 | (*@ predicate p = p *)
                           ^
    Error: Unbound value p
    
|gospel_expected} *)
