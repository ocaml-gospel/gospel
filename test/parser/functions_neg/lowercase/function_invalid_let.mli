(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(*@ function (let) (x : integer) (y : integer) : integer *)

(* {gospel_expected|
[1] File "function_invalid_let.mli", line 11, characters 14-17:
    11 | (*@ function (let) (x : integer) (y : integer) : integer *)
                       ^^^
    Error: Syntax error
    
|gospel_expected} *)
