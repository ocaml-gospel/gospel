(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(*@ type t = { x }*)
(* {gospel_expected|
[1] File "invalid_record_field2.mli", line 11, characters 17-18:
    11 | (*@ type t = { x }*)
                          ^
    Error: Syntax error
    
|gospel_expected} *)
