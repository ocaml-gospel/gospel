(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(*@ type t *)

(*@ axiom ax : forall r. r.t *)

(* {gospel_expected|
[1] File "field_not_type.mli", line 13, characters 27-28:
    13 | (*@ axiom ax : forall r. r.t *)
                                    ^
    Error: Unbound record label t
    
|gospel_expected} *)
