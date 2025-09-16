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
(*@ x, y, x = f ()
    ensures True *)

(* {gospel_expected|
[1] File "duplicate_header_rets3.mli", line 12, characters 10-11:
    12 | (*@ x, y, x = f ()
                   ^
    Error: The variable x is defined twice in this header
    
|gospel_expected} *)
