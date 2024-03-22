(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

type 'a t1 = C of 'a * int
type 'a t2 = T of 'a * int

(*@ function i (a:'a t1): int =
      match a with
      | T (_,y) -> y
*)

(* ERROR:
   Line 17
   Pattern mysmatch 'a t1 with 'b t2
   Replace T by C in line 8
*)

(* {gospel_expected|
   [125] File "t1_not_t2.mli", line 16, characters 8-15:
         16 |       | T (_,y) -> y
                      ^^^^^^^
         Error: This pattern matches values of type 'a40 t2
                but a pattern was expected which matches values of type 'a t1.
   |gospel_expected} *)
