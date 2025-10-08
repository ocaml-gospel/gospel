(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(*@ axiom wrong : forall x : (integer * (integer * integer)).
                    let y, z, w = x in x *)

(* {gospel_expected|
[1] File "let_destruct4.mli", line 12, characters 34-35:
    12 |                     let y, z, w = x in x *)
                                           ^
    Error: Mismatch between type integer * (integer * integer)
           and type 'a * 'b * 'c
    
|gospel_expected} *)
