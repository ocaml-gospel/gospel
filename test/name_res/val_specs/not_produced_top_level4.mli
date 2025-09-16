(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)
module M : sig
  val x : int
end

open M

val f : int -> int
(*@ f y
    ensures x = y
 *)

(* {gospel_expected|
[1] File "not_produced_top_level4.mli", line 18, characters 12-13:
    18 |     ensures x = y
                     ^
    Error: Unbound value x
    
|gospel_expected} *)
