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
exception E2

val f : unit -> unit
(*@ match f () with
    |exception E2 -> ensures True
    |exception E1 -> ensures True
    |exception E1 -> ensures True *)

(* {gospel_expected|
[1] File "duplicate_exn2.mli", line 18, characters 4-33:
    18 |     |exception E1 -> ensures True *)
             ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    Error: The exception E1 is listed twice in this specification
    
|gospel_expected} *)
