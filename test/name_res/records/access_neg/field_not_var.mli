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

(*@ axiom ax : forall r. r.f *)

(* {gospel_expected|
[1] File "field_not_var.mli", line 13, characters 27-28:
    13 | (*@ axiom ax : forall r. r.f *)
                                    ^
    Error: Unbound record label f
    
|gospel_expected} *)
