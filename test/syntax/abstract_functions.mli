(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(*@ open Type_decl *)

(* Parameterless functions *)

(*@ function f1 : integer *)

(*@ function f2 : integer -> integer *)

(*@ function f3 : integer t2 -> integer *)

(*@ function f4 : 'a t2 -> integer *)

(*@ function f5 : 'a t2 -> 'b *)

(*@ function f6 : ('a,'b) t3 -> ('b,'a) t3 *)

(* Functions with parameters *)

(*@ function h1 (x : integer): integer *)

(*@ function h2 (x : integer) (y: integer): integer *)

(*@ function h3 (x : t1) (y: integer t2): integer *)

(*@ function h4 (x : (integer,'a) t3) (y: integer t2): integer *)

(*@ predicate h5 (x : integer) *)

(*@ predicate h6 (x : integer) (y: integer) *)

(*@ predicate h7 (x : t1) (y: integer t2) *)

(*@ predicate h8 (x : (integer,'a) t3) (y: integer t2) *)

(*@ function (!!) (x:integer) : integer *)

(*@ function (??) (x: 'a): 'a *)

(*@ function (!?) (x: 'a) : 'a * 'b *)

(*@ function h9 (x: 'a) : 'a * 'b *)

(*@ function h10 (x: 'a) (y: 'b): 'a * 'b *)

(*@ predicate h11 (x: 'a) (y: 'b) (z: 'c) *)

(*@ function h12 (x: 'a) : 'b -> 'c *)

(*@ function h13 (x: 'a * 'b) (y: 'b * 'a): 'a * 'b *)

(*@ function h14 (x: 'a): test *)

(*@ function h15 (x: test): integer *)
