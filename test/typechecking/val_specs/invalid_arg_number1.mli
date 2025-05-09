(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

val f : int -> unit
(*@ let () = f x y in
      ensures True *)

(* {gospel_expected|
[1] File "invalid_arg_number1.mli", line 11, characters 0-63:
    11 | val f : int -> unit
    12 | (*@ let () = f x y in
    13 |       ensures True *)
    Error: This header has 2 arguments but expected 1
    
|gospel_expected} *)
