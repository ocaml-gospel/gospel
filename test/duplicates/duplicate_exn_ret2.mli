(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

exception E of int * int

val f : int -> unit
(*@ f x
    raises E (x, y)
      ensures True *)

(* {gospel_expected|
[1] File "duplicate_exn_ret2.mli", line 14, characters 6-7:
    14 | (*@ f x
               ^
    Error: The variable x is defined twice in this header
    
|gospel_expected} *)
