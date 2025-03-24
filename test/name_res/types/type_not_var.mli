(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(*@ function f : integer *)

(*@ type t = f *)

(* {gospel_expected|
[1] File "type_not_var.mli", line 13, characters 13-14:
    13 | (*@ type t = f *)
                      ^
    Error: Unbound type constructor f
    
|gospel_expected} *)
