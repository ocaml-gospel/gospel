(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(*@ function f (x : integer) (x : integer) : integer *)

(* {gospel_expected|
[1] File "duplicate_fun_args1.mli", line 11, characters 30-31:
    11 | (*@ function f (x : integer) (x : integer) : integer *)
                                       ^
    Error: Duplicated argument x
    
|gospel_expected} *)
