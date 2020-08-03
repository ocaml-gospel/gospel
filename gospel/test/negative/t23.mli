(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

val f : ('a -> 'b -> 'c) -> 'a -> ('b -> 'c)
(*@ r = f x y *)

(* ERROR:
   Line 12
   insufficient parameters in function header
   add another parameter after y in line 12 *)
