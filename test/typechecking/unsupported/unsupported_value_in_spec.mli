(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

type t = [ `A | `B ]

val x : t
val f : unit -> unit
(*@ modifies x *)

(* {gospel_expected|
[1] File "./unsupported_value_in_spec.mli", line 15, characters 13-14:
    15 | (*@ modifies x *)
                      ^
    Error: Unbound value x
    
|gospel_expected} *)
