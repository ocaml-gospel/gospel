(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

val f : unit -> int
(*@ x, y, z = f ()
    ensures True *)

(* {gospel_expected|
[1] File "invalid_ret_number2.mli", line 11, characters 0-58:
    11 | val f : unit -> int
    12 | (*@ x, y, z = f ()
    13 |     ensures True *)
    Error: This header has 3 return values but expected 1
    
|gospel_expected} *)
