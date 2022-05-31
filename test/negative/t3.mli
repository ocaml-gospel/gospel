(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(*@ function f (x:float): bool = x = 2 *)

(* ERROR Type mismatch float and integer *)

(* EXPECTED
   [125] File "t3.mli", line 11, characters 30-31:
         Error: This term has type `float' but a term was expected of type `integer'.
*)
