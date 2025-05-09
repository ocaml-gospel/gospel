(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

val inc : int -> int
(*@ let y = dec x in
     ensures y = x - 1 *)

(* {gospel_expected|
[1] File "invalid_header_nm.mli", line 12, characters 12-15:
    12 | (*@ let y = dec x in
                     ^^^
    Error: Header name dec does not match the declared value in the OCaml interface
    
|gospel_expected} *)
