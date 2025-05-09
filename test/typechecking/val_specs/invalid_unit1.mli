(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

val f : int -> int
(*@ let () = f x in ensures True *)

(* {gospel_expected|
[1] File "invalid_unit1.mli", line 12, characters 8-10:
    12 | (*@ let () = f x in ensures True *)
                 ^^
    Error: This pattern matches on values of type unit, which is incompatible with int
    
|gospel_expected} *)
