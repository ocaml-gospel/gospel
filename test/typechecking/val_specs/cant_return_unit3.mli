(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

val f : int ref -> unit
(*@ pure *)

(* {gospel_expected|
[1] File "cant_return_unit3.mli", line 12, characters 3-9:
    12 | (*@ pure *)
            ^^^^^^
    Error: This function has no listed side effects, it cannot return unit
    
|gospel_expected} *)
