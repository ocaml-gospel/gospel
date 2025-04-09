(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

type t1

(*@ function test (x : t1) : integer = 0 *)

(* {gospel_expected|
[1] File "ocaml_type5.mli", line 13, characters 23-25:
    13 | (*@ function test (x : t1) : integer = 0 *)
                                ^^
    Error: Unbound type constructor t1
    
|gospel_expected} *)
