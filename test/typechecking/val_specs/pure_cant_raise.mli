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
(*@ f ()
    pure
    raises E
      ensures True *)

(* {gospel_expected|
[1] File "pure_cant_raise.mli", line 13, characters 0-73:
    13 | val f : unit -> unit
    14 | (*@ f ()
    15 |     pure
    16 |     raises E
    17 |       ensures True *)
    Error: A function annotated as pure cannot raise an exception.
    
|gospel_expected} *)
