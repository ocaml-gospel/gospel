(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(*@ type t1 = t2 -> integer and t2 = t3 -> integer and t3 = t1 -> integer *)

(* {gospel_expected|
[1] File "cyclic_alias5.mli", line 11, characters 4-28:
    11 | (*@ type t1 = t2 -> integer and t2 = t3 -> integer and t3 = t1 -> integer *)
             ^^^^^^^^^^^^^^^^^^^^^^^^
    Error: The type abbreviation t1 contains a cycle
           t2 -> t3 -> t1
    
|gospel_expected} *)
