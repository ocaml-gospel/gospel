(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

type t1 = { f : int; g : int }
(*@ type t1 = { f : integer } *)

(*@ axiom ax : forall x. x.g = x.f *)

(* {gospel_expected|
[1] File "ocaml_record3.mli", line 14, characters 27-28:
    14 | (*@ axiom ax : forall x. x.g = x.f *)
                                    ^
    Error: Unbound record label g
    
|gospel_expected} *)
