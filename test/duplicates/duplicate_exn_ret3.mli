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
(*@ f y
    raises E (x, x)
      ensures True *)

(* {gospel_expected|
[1] File "duplicate_exn_ret3.mli", line 15, characters 17-18:
    15 |     raises E (x, x)
                          ^
    Error: The variable x is defined twice in this header
    
|gospel_expected} *)
