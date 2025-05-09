(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

val f : unit -> unit
(*@ pure
    diverges *)

(* {gospel_expected|
[1] File "pure_cant_diverge.mli", line 11, characters 0-45:
    11 | val f : unit -> unit
    12 | (*@ pure
    13 |     diverges *)
    Error: A function marked as pure cannot diverge
    
|gospel_expected} *)
