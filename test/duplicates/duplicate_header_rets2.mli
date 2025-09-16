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
(*@ y, x, x = f ()
    ensures True *)

(* {gospel_expected|
[1] File "duplicate_header_rets2.mli", line 12, characters 10-11:
    12 | (*@ y, x, x = f ()
                   ^
    Error: The variable x is defined twice in this header
    
|gospel_expected} *)
