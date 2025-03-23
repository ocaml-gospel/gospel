(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(*@ type t = { ; } *)
(* {gospel_expected|
[1] File "empty_record2.mli", line 11, characters 15-16:
    11 | (*@ type t = { ; } *)
                        ^
    Error: Syntax error
    
|gospel_expected} *)
