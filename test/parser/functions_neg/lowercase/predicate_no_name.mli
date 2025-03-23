(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(*@ predicate = True *)
(* {gospel_expected|
[1] File "predicate_no_name.mli", line 11, characters 14-15:
    11 | (*@ predicate = True *)
                       ^
    Error: Syntax error
    
|gospel_expected} *)
