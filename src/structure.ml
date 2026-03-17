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
type 'a structure =
  | Tyapp of Id_uast.qualid * 'a list
  | Tyarrow of 'a * 'a
  | Tytuple of 'a list
  | Tvar of Ident.t

(* Built in Gospel types *)
let bool_id = Ident.mk_id "bool"
let prop_id = Ident.mk_id "prop"
let integer_id = Ident.mk_id "integer"
let char_id = Ident.mk_id "char"
let string_id = Ident.mk_id "string"
let float_id = Ident.mk_id "float"
let set_id = Ident.mk_id "set"
let ty_bool = Tyapp (Qid bool_id, [])
let ty_prop = Tyapp (Qid prop_id, [])
let ty_integer = Tyapp (Qid integer_id, [])
let ty_char = Tyapp (Qid char_id, [])
let ty_string = Tyapp (Qid string_id, [])
let ty_float = Tyapp (Qid float_id, [])
let ty_set v = Tyapp (Qid set_id, [ v ])

let primitive_list =
  [
    ("bool", bool_id);
    ("prop", prop_id);
    ("integer", integer_id);
    ("char", char_id);
    ("string", string_id);
    ("float", float_id);
    ("set", set_id);
  ]

let ty_arrow v1 v2 = Tyarrow (v1, v2)

(* Traversal functions required by Inferno *)
let iter f = function
  | Tyapp (_, args) -> List.iter f args
  | Tyarrow (t1, t2) ->
      f t1;
      f t2
  | Tytuple l -> List.iter f l
  | Tvar _ -> ()

let fold f t accu =
  match t with
  | Tyapp (_, args) -> List.fold_left (fun x y -> f y x) accu args
  | Tyarrow (t1, t2) -> f t2 (f t1 accu)
  | Tytuple l -> List.fold_left (fun acc arg -> f arg acc) accu l
  | Tvar _ -> accu

let map f = function
  | Tyapp (id, args) -> Tyapp (id, List.map f args)
  | Tyarrow (t1, t2) -> Tyarrow (f t1, f t2)
  | Tytuple l -> Tytuple (List.map f l)
  | Tvar v -> Tvar v

(* -------------------------------------------------------------------------- *)

(* Traversals at arity 2. *)

exception Iter2

let list_iter2 f ts us =
  if List.length ts <> List.length us then raise Iter2;
  List.iter2 f ts us

let iter2 f t1 t2 =
  match (t1, t2) with
  | Tyapp (id1, args1), Tyapp (id2, args2) ->
      let id1 = Uast_utils.leaf id1 in
      let id2 = Uast_utils.leaf id2 in
      if not (Ident.equal id1 id2) then raise Iter2
      else list_iter2 f args1 args2
  | Tyarrow (arg1, res1), Tyarrow (arg2, res2) ->
      f arg1 arg2;
      f res1 res2
  | Tvar v1, Tvar v2 -> if not (Ident.equal v1 v2) then raise Iter2
  | Tytuple l1, Tytuple l2 -> list_iter2 f l1 l2
  | _ -> raise Iter2

exception InconsistentConjunction = Iter2

(* The function [conjunction] that is expected by the solver is essentially
   [iter2], except it must return a structure, as opposed to a unit value. *)
let conjunction f t u =
  iter2 f t u;
  match t with Tvar _ -> u | _ -> t

(* -------------------------------------------------------------------------- *)

(* Printing. *)

(* open PPrint *)

let pprint _ _ = assert false
