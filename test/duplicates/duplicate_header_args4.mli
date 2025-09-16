(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

val f : int -> int -> unit
(*@ f x x y
    ensures True *)

(* {gospel_expected|
[1] File "duplicate_header_args4.mli", line 12, characters 8-9:
    12 | (*@ f x x y
                 ^
    Error: The variable x is defined twice in this header
    
|gospel_expected} *)
