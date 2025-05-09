(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

val f : int -> int -> unit
(*@ let () = f x [y : integer] in
      ensures True *)

(* {gospel_expected|
[1] File "invalid_arg_number5.mli", line 11, characters 0-82:
    11 | val f : int -> int -> unit
    12 | (*@ let () = f x [y : integer] in
    13 |       ensures True *)
    Error: This header has 1 argument but expected 2
    
|gospel_expected} *)
