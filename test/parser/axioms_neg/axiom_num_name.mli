(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(*@ axiom 1 : True *)
(* {gospel_expected|
[1] File "axiom_num_name.mli", line 11, characters 10-11:
    11 | (*@ axiom 1 : True *)
                   ^
    Error: Syntax error
    
|gospel_expected} *)
