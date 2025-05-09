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
(*@ match f1 () with
    |exception E1 -> ensures True *)

exception E2 of int

val f2 : unit -> unit
(*@ match f2 () with
    |exception E2 -> ensures True
    |exception E1 -> ensures True *)

val f3 : unit -> unit
(*@ match f3 () with
    |exception E2 _ -> ensures True *)

val f4 : unit -> unit
(*@ match f4 () with
    |exception E2 x -> ensures x > 0 *)

exception E3 of int * int

val f5 : unit -> unit
(*@ match f5 () with
    |exception E3 -> ensures True *)

val f6 : unit -> unit
(*@ match f6 () with
    |exception E3 (x, y) -> ensures True *)

val f7 : unit -> unit
(*@ match f7 () with
    |exception E3 _ -> ensures True *)

val f8 : unit -> unit
(*@ match f8 () with
    |exception E1 -> ensures True
    | () -> ensures True *)

val f9 : unit -> int
(*@ match f9 () with
    |exception E1 -> ensures True
    | x -> ensures x = x *)

val f9 : unit -> int
(*@ match f9 () with
    |exception E2 x -> ensures x = x
    | x -> ensures x = x *)

val f10 : unit -> int
(*@ match f10 () with
    |exception E3 (x, y) -> ensures x = y
    |exception E2 x -> ensures x = x
    |exception E1 -> ensures True *)

val f11 : unit -> int
(*@ match f11 () with
    |exception E3 (x, y) -> ensures x = y
    |exception E2 x -> ensures x = x
    |exception E1 -> ensures True
    | x -> ensures x = x *)
