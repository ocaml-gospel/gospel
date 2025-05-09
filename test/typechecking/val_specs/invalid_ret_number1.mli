(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

val f : unit -> int
(*@ let x, y = f () in
      ensures True *)

(* {gospel_expected|
[1] File "invalid_ret_number1.mli", line 11, characters 0-64:
    11 | val f : unit -> int
    12 | (*@ let x, y = f () in
    13 |       ensures True *)
    Error: This header has 2 return values but expected 1
    
|gospel_expected} *)
