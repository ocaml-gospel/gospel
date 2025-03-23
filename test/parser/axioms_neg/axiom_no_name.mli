(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(*@ axiom : True *)
(* {gospel_expected|
[1] File "axiom_no_name.mli", line 11, characters 10-11:
    11 | (*@ axiom : True *)
                   ^
    Error: Syntax error
    
|gospel_expected} *)
