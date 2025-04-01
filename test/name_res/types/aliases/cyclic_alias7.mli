(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(*@ type t1 = (integer * t2)
    and t2 = (integer * t3)
    and t3 = (integer * t1) *)

(* {gospel_expected|
[1] File "cyclic_alias7.mli", line 11, characters 4-33:
    11 | ....type t1 = (integer * t2)
    12 |     .......................
    Error: The type abbreviation t1 contains a cycle
           t2 -> t3 -> t1
    
|gospel_expected} *)
