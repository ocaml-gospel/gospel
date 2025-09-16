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
(*@ f x
    ensures True *)

(* {gospel_expected|
[1] File "invalid_arg_number4.mli", line 11, characters 0-54:
    11 | val f : int -> int -> unit
    12 | (*@ f x
    13 |     ensures True *)
    Error: This header has 1 argument but expected 2
    
|gospel_expected} *)
