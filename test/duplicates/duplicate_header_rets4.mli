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
(*@ let x, x, y = f () in ensures True *)

(* {gospel_expected|
[1] File "duplicate_header_rets4.mli", line 12, characters 11-12:
    12 | (*@ let x, x, y = f () in ensures True *)
                    ^
    Error: The variable x is defined twice in this header
    
|gospel_expected} *)
