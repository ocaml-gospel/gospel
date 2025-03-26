(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(*@ type t = { y : integer } *)

(*@ predicate test (r : t) = r.y = r.x *)

(* {gospel_expected|
[1] File "unbound_field4.mli", line 13, characters 37-38:
    13 | (*@ predicate test (r : t) = r.y = r.x *)
                                              ^
    Error: Unbound record label x
    
|gospel_expected} *)
