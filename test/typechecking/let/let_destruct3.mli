(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(*@ axiom wrong : forall x : integer. let y : bool = x in False *)

(* {gospel_expected|
[1] File "let_destruct3.mli", line 11, characters 42-50:
    11 | (*@ axiom wrong : forall x : integer. let y : bool = x in False *)
                                                   ^^^^^^^^
    Error: Mismatch between type bool and type integer
    
|gospel_expected} *)
