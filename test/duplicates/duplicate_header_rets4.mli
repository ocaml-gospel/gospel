(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

val f : unit -> int * int * int
(*@ x, x, y = f ()
    ensures True *)

(* {gospel_expected|
[1] File "duplicate_header_rets4.mli", line 12, characters 7-8:
    12 | (*@ x, x, y = f ()
                ^
    Error: The variable x is defined twice in this header
    
|gospel_expected} *)
