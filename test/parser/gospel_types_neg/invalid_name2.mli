(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(*@ type 1 *)
(* {gospel_expected|
[1] File "invalid_name2.mli", line 11, characters 9-10:
    11 | (*@ type 1 *)
                  ^
    Error: Syntax error
    
|gospel_expected} *)
