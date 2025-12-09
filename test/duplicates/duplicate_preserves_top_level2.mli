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
(*@ preserves x
    preserves y
    preserves x *)

(* {gospel_expected|
[1] File "duplicate_preserves_top_level2.mli", line 11, characters 4-5:
    11 | val x : int ref
             ^
    Error: The variable x is listed as preserved twice
    
|gospel_expected} *)
