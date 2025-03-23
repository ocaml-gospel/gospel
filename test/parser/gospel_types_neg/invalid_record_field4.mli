(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(*@ type t = { x : integer; x }*)
(* {gospel_expected|
[1] File "invalid_record_field4.mli", line 11, characters 30-31:
    11 | (*@ type t = { x : integer; x }*)
                                       ^
    Error: Syntax error
    
|gospel_expected} *)
