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
(*@ let () = f x y z in
      ensures True *)

(* {gospel_expected|
[1] File "invalid_arg_number3.mli", line 11, characters 0-72:
    11 | val f : int -> int -> unit
    12 | (*@ let () = f x y z in
    13 |       ensures True *)
    Error: This header has 3 arguments but expected 2
    
|gospel_expected} *)
