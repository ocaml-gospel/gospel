(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

exception E1

val f1 : unit -> unit
(*@ f1 ()
    raises E1 *)

exception E2 of int

val f2 : unit -> unit
(*@ f2 ()
    raises E2
    raises E1 *)

val f3 : unit -> unit
(*@ f3 ()
    raises E2 *)

val f4 : unit -> unit
(*@ f4 ()
    raises E2 x
      ensures x > 0 *)

exception E3 of int * int

val f5 : unit -> unit
(*@ f5 ()
    raises E3 *)

val f6 : unit -> unit
(*@ f6 ()
    raises E3 *)

val f7 : unit -> unit
(*@ f7 ()
    raises E3 *)

val f8 : unit -> unit
(*@ f8 ()
    raises E1 *)

val f9 : unit -> int
(*@ x = f9 ()
    ensures x = x
    raises E1 *)

val f9 : unit -> int
(*@ x = f9 ()
    ensures x = x
    raises E2 x
      ensures x = x *)

val f10 : unit -> int
(*@ f10 ()
    raises E3 (x, y)
      ensures x = y
    raises E2 x
      ensures x = x
    raises E1 *)

val f11 : unit -> int
(*@ x = f11 ()
    ensures x = x
    raises E3 (x, y)
      ensures x = y
    raises E2 x
      ensures x = x
    raises E1 *)
