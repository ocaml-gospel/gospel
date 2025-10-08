(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(*@ axiom test : forall x : ('a * 'b * 'b). let x, y, z = x in x = y *)

(* {gospel_expected|
[1] File "let_destruct7.mli", line 11, characters 67-68:
    11 | (*@ axiom test : forall x : ('a * 'b * 'b). let x, y, z = x in x = y *)
                                                                            ^
    Error: Mismatch between type 'b and type 'a
    
|gospel_expected} *)
