(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)
open Uast

let mk_term t l = { term_desc = t; term_loc = l }

(** [mk_op_apply op arg1 arg2] Creates a term for the application of the
    operator [op] to the list of arguments [args]. Although this function could
    be used for any function application, the locations of the subterms would
    not correspond to what would be expected. *)
let mk_op_apply op args =
  let f = mk_term (Tpreid (Qpreid op)) op.pid_loc in
  let t =
    List.fold_left (fun f arg -> mk_term (Tapply (f, arg)) arg.term_loc) f args
  in
  t.term_desc

(** If [t] is an application of an infix operation using the [Tinfix]
    constructor, then [chain t] desugars it so that it uses the [Tidapp]
    constructor. If [t] is a chain of infix operators (e.g. 3 < 4 <= 5 > ...)
    then [infix t] desugars it into a conjuction of [Tidapp] constructors so
    that the term represents [3 < 4 /\ 4 <= 5 /\ 5 > ...]. If [t] is not a chain
    of infix operators, then this function returns [t] unchanged.

    This function is useful for typechecking since it normalizes infix
    operations into using the same constructor as normal function applications,
    but also necessary during parsing to mark the end of a chain of infix
    operators. For example: without this function [3 < (4 < 5)] would be
    represented the same way as [3 < 4 < 5]. To avoid this, we use this function
    to end the chain [Infix] constructors. *)
let rec chain t =
  let mk_term t loc = { term_desc = t; term_loc = loc } in
  match t.term_desc with
  | Tinfix (t1, o, t2) -> (
      match t2.term_desc with
      (* We match on the right subtree due to the associativity of
         infix operators. *)
      | Tinfix (t3, _, _) ->
          (* If we reach this case, then the term [t] is [t1 < t2] where [t2] =
             [t3 <= ...] and [<] and [<=] stand for arbitrary infix
             operators. In this case, we create the conjunction [t1 < t3 /\
             t2] *)
          let t = chain t2 in
          let mk_pid = Preid.create ~attrs:[] ~loc:Location.none in
          let qconj = mk_pid "infix /\\" in
          let infix = mk_term (mk_op_apply o [ t1; t3 ]) t.term_loc in
          mk_term (mk_op_apply qconj [ infix; t ]) Location.none
      | _ ->
          (* If the right subtree is not another infix operation, we
            translate the term naturally*)
          mk_term (mk_op_apply o [ t1; t2 ]) t.term_loc)
  | _ -> t

(** Same behaviour as [chain] but receives and retruns a [term_desc] *)
let chain_desc t = (chain { term_desc = t; term_loc = Location.none }).term_desc

let prefix s = "prefix " ^ s
let infix s = "infix " ^ s
let mixfix s = "mixfix " ^ s
