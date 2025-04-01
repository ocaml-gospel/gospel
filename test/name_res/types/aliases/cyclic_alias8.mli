(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(*@ type t1 = (integer * integer) -> t2 sequence
    and t2 = (integer * integer) -> t3 sequence
    and t3 = (integer * integer) -> t4 sequence
    and t4 = (integer * integer) -> t1 sequence *)

(* {gospel_expected|
[1] File "cyclic_alias8.mli", line 11, characters 4-53:
    11 | ....type t1 = (integer * integer) -> t2 sequence
    12 |     ...........................................
    Error: The type abbreviation t1 contains a cycle
           t2 -> t3 -> t4 -> t1
    
|gospel_expected} *)
