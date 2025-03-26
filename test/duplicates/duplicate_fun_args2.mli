(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(*@ function f (y : integer) (x : integer) (x : integer) : integer *)

(* {gospel_expected|
[1] File "duplicate_fun_args2.mli", line 11, characters 44-45:
    11 | (*@ function f (y : integer) (x : integer) (x : integer) : integer *)
                                                     ^
    Error: Duplicated argument x
    
|gospel_expected} *)
