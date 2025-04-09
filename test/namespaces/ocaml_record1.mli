(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

type t1 = { f : int }

(*@ axiom ax : forall x. x.f = x.f *)

(* {gospel_expected|
[1] File "ocaml_record1.mli", line 13, characters 33-34:
    13 | (*@ axiom ax : forall x. x.f = x.f *)
                                          ^
    Error: Unbound record label f
    
|gospel_expected} *)
