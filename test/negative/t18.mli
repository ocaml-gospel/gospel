(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

val f : z:int -> int -> int
(*@ r = f ~x y*)

(* ERROR:
   Line 12
   first parameter does not match the name in the type
   remove replace x by z in line 12 *)
