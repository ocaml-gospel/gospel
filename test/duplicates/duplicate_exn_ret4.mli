(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

exception E of int * int

val f : int -> unit
(*@ match f y with
    |exception E (x, x) -> ensures True *)

(* {gospel_expected|
[1] File "duplicate_exn_ret4.mli", line 15, characters 21-22:
    15 |     |exception E (x, x) -> ensures True *)
                              ^
    Error: The variable x is defined twice in this header
    
|gospel_expected} *)
