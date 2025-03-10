(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(** This module contains a typechecker that uses the Inferno library, which
    handles type inference. We first give definitions for program variables and
    types. We then feed these into the [Solver] functor which allows us to solve
    type constraints. *)

module X = Ident
module T = Types
module S = Structure
open Inferno
open Id_uast
open Tast
module Solver = Solver.Make (X) (S) (T)
open Solver
module W = Warnings

(** Deep type for functions *)
let deep_arrow arg ret =
  let f = S.ty_arrow arg ret in
  DeepStructure f

(** [map_binders f xs] is conceptually similar to [List.map], but creates a
    binder for each element in [xs]. This means an application of [f] may bind
    (and possibly return) inferno variables as well as applying some arbitrary
    transformation, as one would expect from [List.map]. This function returns a
    binder that introduces a list of all the values bound by applying [f] to
    every element in [xs]. *)
let rec map_binders (f : 'a -> ('b, 'r) binder) (xs : 'a list) :
    ('b list, 'r) binder =
 fun k ->
  match xs with
  | [] -> k []
  | x :: xs ->
      let@ y = f x in
      let@ l = map_binders f xs in
      k (y :: l)

(* The following functions are used to turn Gospel signatures into Inferno
   constraints that, when solved, will produce typed signatures. If the
   constraint is unsatisfiable, then Inferno will produce an exception which we
   convert into an appropriate Gospel error. *)

(* All Inferno constraints have a semantic value associated with
   them. If a constraint is satisfiable, then Inferno will produce the
   associated sematic value. E.g. the [hastype] function produces a
   constraint whose semantic value is a typed term. This means if
   Inferno can solve the constraint, then it will build a typed
   term. *)

(** [pty_to_deep inferno_vars f t] maps the type [t] to a deep type. Since the
    translation of type variables depends on whether this is the type of a top
    level function or a type annotation for a local variable, this function
    allows the caller to supply an [f] to detail how to handle type variables.
*)
let rec pty_to_deep f pty =
  let pty_to_deep = pty_to_deep f in
  match pty with
  | PTtyvar id -> f id
  | PTtyapp (id, l) ->
      DeepStructure (S.Tyapp (Types.leaf id, List.map pty_to_deep l))
  | PTarrow (arg, ret) -> deep_arrow (pty_to_deep arg) (pty_to_deep ret)
  | PTtuple _ -> assert false

(** [pty_to_deep_rigid pty] is a refinement of [pty_to_deep] where each type
    variable in [pty] becomes a rigid type variable i.e. this variable cannot be
    unified with any other type. *)
let pty_to_deep_rigid = pty_to_deep (fun v -> DeepStructure (Tvar v))

(** [pty_to_deep_flex vars pty] is a refinement of [pty_to_deep] where each type
    variable in [pty] is mapped to the corresponding inferno variable in the
    associative list [vars]. This way, assuming that all the inferno variables
    in [vars] are flexible, the type variables in the resulting deep type can be
    unified with any other type *)
let pty_to_deep_flex vars =
  pty_to_deep (fun v -> DeepVar (List.assoc v.id_tag vars))

(** [assoc_vars vars] returns a binder for a list where every type variable in
    [vars] is associated with a fresh Inferno type variable. *)
let assoc_vars vars =
  let f var k =
    let@ v = exist in
    k (var.Ident.id_tag, v)
  in
  map_binders f vars

(** [top_level_pty env id] returns a binder that introduces the type of the top
    level function [id]. *)
let top_level_pty params ty =
 fun k ->
  (* Associate each type variable in [params] with a flexible inferno
      variable. *)
  let@ vars = assoc_vars params in
  (* Turn the type [ty] into an inferno type variable *)
  let@ ty = deep (pty_to_deep_flex vars ty) in
  k ty

(** [build_def l c] Returns a constraint where the variables in [l] have been
    added to the scope of constraint [c] by means of a chain of [def]
    constraints. Each element of [l] is a pair consisting of an identifier [x]
    and an Inferno binder which introduces the type of [x] into a constraint. *)
let build_def l c =
  (* [loop (id, ty) c] wraps the constraint [c] in a [def] constraint
     where the variable [id] has the type associated with the binder
     [ty]. *)
  let loop (id, ty) c =
    let@ ty_var = ty in
    let+ l, t = def id ty_var c and+ ty = decode ty_var in
    (mk_ts id ty :: l, t)
  in
  (* Map the constraint [c] to one where the semantic value is a pair whose
     first element is an empty list and the second is the semantic value of [c]
     so we can build the list of typed arguments while keeping the previous
     value unchanged. *)
  let acc =
    let+ t = c in
    ([], t)
  in
  List.fold_right loop l acc

(** [hastype ts t r] receives an untyped term [t] and the expected type [r] and
    produces a constraint whose semantic value is a typed term. The environment
    [ts] is used to ensure that all type annotations are valid. *)
let rec hastype (t : Id_uast.term) (r : variable) =
  (* This line ensures that Inferno errors refer to the correct code
     fragment. *)
  Solver.correlate (t.term_loc.loc_start, t.term_loc.loc_end)
  @@
  let+ t_node =
    match t.term_desc with
    | Id_uast.Ttrue ->
        (* For true and false, we state that the expected type must be
          a boolean *)
        let+ () = r --- S.ty_bool in
        Ttrue
    | Tfalse ->
        let+ () = r --- S.ty_bool in
        Tfalse
    | Tconst constant ->
        (* Depending on the type of constant, we restrict the expected type
           accordingly *)
        let+ () =
          match constant with
          | Pconst_integer _ -> r --- S.ty_integer
          | Pconst_char _ -> r --- S.ty_char
          | Pconst_string _ -> r --- S.ty_string
          | Pconst_float _ -> r --- S.ty_float
        in
        Tconst constant
    | Tlocal id ->
        (* If the variable is defined within the scope of this term,
          we create a constraint stating that the variable [id]
          must be an instance of type [r]. No lookup is performed
          as the bookkeeping is done by Inferno. *)
        let+ _ = instance id r in
        (* We can ignore the return value of [instance] since it will always
          return the empty list since all local variables are bound with
          [def], which creates a monomorphic scheme. *)
        Tvar (Qid id)
    | Tvar (q, params, pty) ->
        (* In the case of a top level definition outside the scope of this term,
           we use the type provided during the name resolution stage. *)
        let@ ty = top_level_pty params pty in
        let+ () = r -- ty in
        Tvar q
    | Tlet (id, t1, t2) ->
        (* let id = t1 in t2 *)
        (* The term [t1] has some arbitrary type [v_type]. *)
        let@ v_type = exist in
        let+ t1 = hastype t1 v_type
        and+ t2 =
          (* Create a constraint for the term [t2] where the variable
            [id] has type [v_type]. *)
          let t2 = hastype t2 r in
          def id v_type t2
        in
        Tlet (id, t1, t2)
    | Tapply (t1, t2) ->
        (* t1 t2 (Function application)*)
        (* Inferno variables for the function's argument and return type. *)
        let@ arg_ty = exist in
        let@ res = exist in
        (* The term [t1] must be of the function type [arg_ty -> r]. *)
        let+ t1 = lift hastype t1 (S.ty_arrow arg_ty res)
        (* The argument [t2] has some type [arg_ty]. *)
        and+ t2 = hastype t2 arg_ty
        (* The return value of the term has the same type of the return value of
           the function.

           Note: Although [res] could be inlined with [r] with no functional
           difference in terms of typechecking, doing it this way forces Inferno
           to first solve the previous two constraints meaning we will get a more
           precise error message when the return type is invalid. If this is
           unclear, try replacing [res] with [r] and see what happens. *)
        and+ () = r -- res in
        Tapply (t1, t2)
    | Tquant (q, l, t) ->
        (* forall. x y z. t *)
        (* The term [t] must be a formula *)
        let c = lift hastype t S.ty_bool in
        (* If there is a type annoation, transform it into a deep type
           and create a binder for it. *)
        let binder_to_deep = function
          | None -> exist
          | Some ty -> fun k -> deep (pty_to_deep_rigid ty) k
        in
        (* Transform the list of Gospel type annotation into a list of
           Inferno binders *)
        let l = List.map (fun (x, b) -> (x, binder_to_deep b)) l in
        let+ l, t = build_def l c in
        Tquant (q, l, t)
    | Tif (g, then_b, else_b) ->
        (* if g then then_b else else_b *)
        (* The guard must have type [bool] *)
        let+ g = lift hastype g S.ty_bool
        (* Both branches must have the return type [r] *)
        and+ then_b = hastype then_b r
        and+ else_b = hastype else_b r in
        Tif (g, then_b, else_b)
    | _ -> assert false
  (* By calling [decode], we can get the inferred type of the term. *)
  and+ t_ty = decode r in
  mk_term t_node t_ty t.term_loc

(** Solves an arbitrary constraint assuming types are not allowed to be
    recursive *)
let typecheck c =
  try snd (Solver.solve ~rectypes:false (let0 c)) with
  | Solver.Unify (loc, ty1, ty2) ->
      let ty1s = Fmt.str "%a" Types.print_ty ty1 in
      let ty2s = Fmt.str "%a" Types.print_ty ty2 in
      let loc = Uast_utils.mk_loc loc in
      W.error ~loc (W.Bad_type (ty1s, ty2s))
  | Solver.Unbound _ -> assert false
(* Unbound variables are caught before we run the solver *)

let process_fun_spec f =
  Option.fold ~some:(fun _ -> assert false) ~none:(pure None) f

(** [function_cstr ts f] Creates a constraint whose semantic value is a list of
    signatures whose head is the declaration of the function described by [f].
    This is the conjunction of two constraints: one that checks if the body of
    the function is well typed and another that checks if the function's
    specification is well typed. *)
let function_cstr (f : Id_uast.function_) : Tast.function_ co =
  (* Turn the return type into a deep type *)
  let ret_pty = Option.value ~default:Types.ty_bool f.fun_type in
  let arrow_ty =
    List.fold_right
      (fun (_, pty) acc -> PTarrow (pty, acc))
      f.fun_params ret_pty
  in

  let@ ret_ty = deep (pty_to_deep_rigid ret_pty) in

  (* Map each type annotation of a parameter to a deep type. *)
  let to_deep (arg, pty) =
    let deep_arg = pty_to_deep_rigid pty in
    (arg, deep_arg)
  in
  let deep_params = List.map to_deep f.fun_params in

  (* The function type encoded as a deep type *)
  let deep_fun_type = pty_to_deep_rigid arrow_ty in
  let@ fun_ty = deep deep_fun_type in

  (* Typecheck the body of the function. Must have the same return
     type that the user indicated. *)
  let body_c =
    match f.fun_def with
    | None -> pure None
    | Some t ->
        let c = hastype t ret_ty in
        let+ tt =
          if f.fun_rec then
            (* If the function is recursive, it must be defined within
               the scope of the term. *)
            def f.fun_name fun_ty c
          else c
        in
        Some tt
  in
  (* Each variable is now associated with a binder. *)
  let deep_params = List.map (fun (x, dty) -> (x, deep dty)) deep_params in

  (* Typed term and function parameters *)
  let+ params, tt = build_def deep_params body_c
  (* Typed function specification *)
  and+ fun_spec = process_fun_spec f.fun_spec
  (* Decoded return type *)
  and+ ret = decode ret_ty in
  let fun_ty = Option.fold ~some:(fun t -> t.t_ty) ~none:ret tt in
  mk_function f params tt fun_ty fun_spec

(** Creates a constraint ensuring the term within an axiom has type [prop]. *)
let axiom_cstr ax =
  let@ ty = shallow S.ty_bool in
  let+ t = hastype ax.Id_uast.ax_term ty in
  mk_axiom ax.ax_name t ax.ax_loc ax.ax_text

let axiom ax = typecheck (axiom_cstr ax)
let function_ f = typecheck (function_cstr f)
