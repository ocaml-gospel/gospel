(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(*@ type = integer *)
(* {gospel_expected|
[1] File "no_name.mli", line 11, characters 9-10:
    11 | (*@ type = integer *)
                  ^
    Error: Syntax error
    
|gospel_expected} *)
