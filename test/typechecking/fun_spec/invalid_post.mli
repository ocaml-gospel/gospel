(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(*@ function f (x : integer) : integer
      ensures x *)
(* {gospel_expected|
[1] File "invalid_post.mli", line 12, characters 14-15:
    12 |       ensures x *)
                       ^
    Error: Mismatch between type integer and type prop
    
|gospel_expected} *)
