(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(* This file is intended to be used with GOSPEL-framework only.
   It is automatically opened (and then ocamlstdlib.mli as well).

   The following are built-in in GOSPEL:

   type unit
   type string
   type float
   type bool
   type integer

   type 'a option
   function None: 'a option
   function Some (x: 'a) : 'a option

   type 'a list
   function ([]): 'a list
   function (::) (x: 'a) (l: 'a list) : 'a list

   predicate (=) (x y: 'a)

*)

(*@ function (+)   (x y: integer) : integer *)
(*@ function (-)   (x y: integer) : integer *)
(*@ function ( * ) (x y: integer) : integer *)
(*@ function (/)   (x y: integer) : integer *)
(*@ function mod   (x y: integer) : integer *) (* TODO allow infix in the parser*)
(*@ function (-_)  (x: integer) : integer *)
(*@ predicate (>)  (x y: integer) *)
(*@ predicate (>=) (x y: integer) *)
(*@ predicate (<)  (x y: integer) *)
(*@ predicate (<=) (x y: integer) *)
