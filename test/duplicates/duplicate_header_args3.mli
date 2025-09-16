(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

val f : int -> int -> unit
(*@ f x y x
    ensures True *)

(* {gospel_expected|
[1] File "duplicate_header_args3.mli", line 12, characters 10-11:
    12 | (*@ f x y x
                   ^
    Error: The variable x is defined twice in this header
    
|gospel_expected} *)
