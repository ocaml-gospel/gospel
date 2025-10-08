(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(*@ axiom test : forall x : ('a * 'b * 'c). let x, (y, z) = x in True *)

(* {gospel_expected|
[1] File "let_destruct5.mli", line 11, characters 60-61:
    11 | (*@ axiom test : forall x : ('a * 'b * 'c). let x, (y, z) = x in True *)
                                                                     ^
    Error: Mismatch between type 'a * 'b * 'c and type 'd * ('e * 'f)
    
|gospel_expected} *)
