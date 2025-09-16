(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

val f : int -> int
(*@ y = f x
    consumes y
 *)

(* {gospel_expected|
[1] File "consume_ret.mli", line 13, characters 13-14:
    13 |     consumes y
                      ^
    Error: Unbound value y
    
|gospel_expected} *)
