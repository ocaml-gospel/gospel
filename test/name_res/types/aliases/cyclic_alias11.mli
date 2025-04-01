(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(*@ type t
    and t1 = (integer * t) -> t2 sequence
    and t2 = (integer * t) -> t1 sequence
    and t3 = (integer * t) -> t4 sequence
    and t4 = (integer * t) -> t3 sequence *)

(* {gospel_expected|
[1] File "cyclic_alias11.mli", line 12, characters 4-46:
    12 | ....and t1 = (integer * t) -> t2 sequence
    13 |     .....................................
    Error: The type abbreviation t1 contains a cycle
           t2 -> t1
    
|gospel_expected} *)
