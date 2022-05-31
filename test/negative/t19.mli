(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

val f : y:int -> int -> int
(*@ r = f ~y y*)

(* ERROR:
   Line 12
   duplicated vars in val header
   remove replace the second y by z in line 12 *)

(* EXPECTED
   [125] File "t19.mli", line 12, characters 10-11:
         Error: The variable `y' is duplicated in this pattern.
*)
