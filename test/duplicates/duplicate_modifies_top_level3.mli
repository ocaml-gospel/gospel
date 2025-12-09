(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

val x : int ref
val y : int ref
val f : unit -> unit
(*@ modifies x
    modifies x
    modifies y *)

(* {gospel_expected|
[1] File "duplicate_modifies_top_level3.mli", line 11, characters 4-5:
    11 | val x : int ref
             ^
    Error: The variable x is listed as modified twice
    
|gospel_expected} *)
