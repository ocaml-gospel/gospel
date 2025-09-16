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

val f : int -> int
(*@ f y
    ensures M.x = y
 *)

(* {gospel_expected|
[1] File "not_produced_top_level3.mli", line 16, characters 14-15:
    16 |     ensures M.x = y
                       ^
    Error: Unbound value M.x
    
|gospel_expected} *)
