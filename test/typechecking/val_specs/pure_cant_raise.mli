(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

exception E

val f : unit -> unit
(*@ pure
    match f () with
    |exception E -> ensures True *)

(* {gospel_expected|
[1] File "pure_cant_raise.mli", line 13, characters 0-85:
    13 | val f : unit -> unit
    14 | (*@ pure
    15 |     match f () with
    16 |     |exception E -> ensures True *)
    Error: A function annotated as pure cannot raise an exception.
    
|gospel_expected} *)
