(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

val f : ?y:int -> int -> int
(*@ r = f y x *)

(* ERROR:
   Line 12
   the first parameter should be marked as optional in spec header
   add ~ before y in line 12 *)
