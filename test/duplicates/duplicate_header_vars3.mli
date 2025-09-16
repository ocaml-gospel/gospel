(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

val f : int -> int -> int
(*@ x = f y x
    ensures True *)

(* {gospel_expected|
[1] File "duplicate_header_vars3.mli", line 12, characters 12-13:
    12 | (*@ x = f y x
                     ^
    Error: The variable x is defined twice in this header
    
|gospel_expected} *)
