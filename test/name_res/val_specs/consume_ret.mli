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
(*@ consumes y
    let y = f x
 *)

(* {gospel_expected|
[1] File "consume_ret.mli", line 12, characters 13-14:
    12 | (*@ consumes y
                      ^
    Error: Unbound value y
    
|gospel_expected} *)
