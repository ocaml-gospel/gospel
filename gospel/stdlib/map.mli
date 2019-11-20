(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(* the type ('a, 'b) map is defined internally in GOSPEL and can be
   written as 'a -> 'b *)

(*@ function ( [<-] ) (m: 'a -> 'b) (x:'a) (y: 'b) : 'a -> 'b *)

(*@ function ( [_] ) (m: 'a -> 'b) (x: 'a) : 'b *)
