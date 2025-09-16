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

val f : unit -> unit
(*@ f ()
    raises E1
    raises E1 *)

(* {gospel_expected|
[1] File "duplicate_exn1.mli", line 16, characters 4-13:
    16 |     raises E1 *)
             ^^^^^^^^^
    Error: The exception E1 is listed twice in this specification
    
|gospel_expected} *)
