(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(** This module contains the definition of an Inferno [structure], which in the
    case of our type checker is a type with unresolved type variables. *)

(** Represents a Gospel type where unresolved type variables are represented by
    values of type ['a]. *)
type 'a structure = Tyapp of Ident.t * 'a list | Tyarrow of 'a * 'a

let create_id s = Ident.mk_id s Location.none

(* Built in Gospel types *)
let bool_id = create_id "bool"
let integer_id = create_id "integer"
let char_id = create_id "char"
let string_id = create_id "string"
let float_id = create_id "float"
let sequence_id = create_id "sequence"
let ty_bool = Tyapp (bool_id, [])
let ty_integer = Tyapp (integer_id, [])
let ty_char = Tyapp (char_id, [])
let ty_string = Tyapp (string_id, [])
let ty_float = Tyapp (float_id, [])

let primitive_list =
  [
    ("bool", bool_id);
    ("integer", integer_id);
    ("char", char_id);
    ("string", string_id);
    ("float", float_id);
    ("sequence", sequence_id);
  ]

let ty_arrow v1 v2 = Tyarrow (v1, v2)

(* Traversal functions required by Inferno *)
let iter f = function
  | Tyapp (_, args) -> List.iter f args
  | Tyarrow (t1, t2) ->
      f t1;
      f t2

let fold f t accu =
  match t with
  | Tyapp (_, args) -> List.fold_left (fun x y -> f y x) accu args
  | Tyarrow (t1, t2) -> f t2 (f t1 accu)

let map f = function
  | Tyapp (id, args) -> Tyapp (id, List.map f args)
  | Tyarrow (t1, t2) -> Tyarrow (f t1, f t2)

(* -------------------------------------------------------------------------- *)

(* Traversals at arity 2. *)

exception Iter2

let list_iter2 f ts us =
  if List.length ts <> List.length us then raise Iter2;
  List.iter2 f ts us

let iter2 f t1 t2 =
  match (t1, t2) with
  | Tyapp (id1, args1), Tyapp (id2, args2) ->
      if not (Ident.equal id1 id2) then raise Iter2
      else list_iter2 f args1 args2
  | Tyarrow (arg1, res1), Tyarrow (arg2, res2) ->
      f arg1 arg2;
      f res1 res2
  | _ -> raise Iter2

exception InconsistentConjunction = Iter2

(* The function [conjunction] that is expected by the solver is essentially
   [iter2], except it must return a structure, as opposed to a unit value. *)
let conjunction f t u =
  iter2 f t u;
  t

(* -------------------------------------------------------------------------- *)

(* Printing. *)

(* open PPrint *)

let pprint _ _ = assert false
