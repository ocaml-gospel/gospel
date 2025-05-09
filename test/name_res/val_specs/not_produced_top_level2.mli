(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

val x : int
val f : int -> int
(*@ let _ = f y in
      ensures x = y
 *)

(* {gospel_expected|
[1] File "not_produced_top_level2.mli", line 14, characters 14-15:
    14 |       ensures x = y
                       ^
    Error: Unbound value x
    
|gospel_expected} *)
