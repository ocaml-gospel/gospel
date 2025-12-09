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
val f : unit -> unit
(*@ modifies x
    modifies x *)

(* {gospel_expected|
[1] File "duplicate_modifies_top_level1.mli", line 14, characters 13-14:
    14 |     modifies x *)
                      ^
    Error: The variable x is listed as modified twice
    
|gospel_expected} *)
