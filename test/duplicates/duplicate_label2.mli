(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(*@ type t = { y : integer; x : integer; x : integer } *)

(* {gospel_expected|
[1] File "duplicate_label2.mli", line 11, characters 41-42:
    11 | (*@ type t = { y : integer; x : integer; x : integer } *)
                                                  ^
    Error: Two labels are named x
    
|gospel_expected} *)
