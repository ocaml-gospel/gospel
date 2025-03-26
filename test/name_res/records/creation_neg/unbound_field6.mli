(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(*@ type t = { y : integer; z : integer } *)

(*@ axiom ax : forall r : t. { x = 0; y = 0 } *)

(* {gospel_expected|
[1] File "unbound_field6.mli", line 13, characters 29-45:
    13 | (*@ axiom ax : forall r : t. { x = 0; y = 0 } *)
                                      ^^^^^^^^^^^^^^^^
    Error: No record found with the provided labels
    
|gospel_expected} *)
