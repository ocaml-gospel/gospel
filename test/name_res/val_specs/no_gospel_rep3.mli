(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

type t
(*@ mutable *)

val f : t -> unit
(*@ modifies x
    requires x = x
    let () = f x *)

(* {gospel_expected|
[1] File "no_gospel_rep3.mli", line 16, characters 17-18:
    16 |     requires x = x
                          ^
    Error: Unbound value x
    
|gospel_expected} *)
