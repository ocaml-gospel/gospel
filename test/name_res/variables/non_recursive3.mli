(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(*@ predicate p (n : integer) = p *)

(* {gospel_expected|
[1] File "non_recursive3.mli", line 11, characters 32-33:
    11 | (*@ predicate p (n : integer) = p *)
                                         ^
    Error: Unbound value p
    
|gospel_expected} *)
