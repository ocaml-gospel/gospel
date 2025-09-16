(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

val f : int -> int
(*@ x = f x
    ensures True *)

(* {gospel_expected|
[1] File "duplicate_header_vars1.mli", line 12, characters 10-11:
    12 | (*@ x = f x
                   ^
    Error: The variable x is defined twice in this header
    
|gospel_expected} *)
