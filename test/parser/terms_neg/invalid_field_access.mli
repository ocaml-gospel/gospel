(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(*@ type t = { f : prop }*)

(*@ axiom test : forall x. x..f *)
(* {gospel_expected|
[1] File "invalid_field_access.mli", line 13, characters 28-30:
    13 | (*@ axiom test : forall x. x..f *)
                                     ^^
    Error: Syntax error
    
|gospel_expected} *)
