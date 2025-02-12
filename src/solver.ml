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

let leaf q = match q with Qid id -> id | Qdot (_, id) -> id

(* The following functions make use of "deep types". These are a
   special encoding provided by Inferno when we already know a type
   before we begin typechecking. This is useful for logical functions
   and predicates since these are required to have arguments with
   explicitly annotated types (since Gospel does not support let
   polymophism). After constructing a deep type, we then call the
   [deep] function which returns a variable [x] and creates an Inferno
   constraint stating that [x] must be equal to the provided deep
   type. *)

(** Deep type for functions *)
let deep_arrow arg ret =
  let f = S.ty_arrow arg ret in
  DeepStructure f

let deep_bool = DeepStructure S.ty_bool

(** [pty_to_deep ts pty] Transforms the type annotation [pty] into a deep type.
    Also checks if the identifiers within the type are valid i.e. have been
    defined. *)
let rec pty_to_deep = function
  | PTtyapp (Qid id, l) -> DeepStructure (S.Tyapp (id, List.map pty_to_deep l))
  | PTarrow (pty1, pty2) ->
      let dty1 = pty_to_deep pty1 in
      let dty2 = pty_to_deep pty2 in
      DeepStructure (S.ty_arrow dty1 dty2)
  | _ -> assert false

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

(** [build_def l c] Returns a constraint where the variables in [l] have been
    added to the scope of constraint [c] by means of a chain of [def]
    constraints. Each element of [l] is a pair consisting of an identifier [x]
    and an Inferno binder which introduces the type of [x] into a constraint.
    @raise _ if there are duplicate arguments in [l]. *)
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
    [ts] is used to ensure that all type annotations are valid. No environment
    is used to keep track of what variables are in scope since that is done by
    Inferno. *)
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
    | Tvar q ->
        let id = leaf q in
        (* The identifier must be a variable of type [r]. We do not
           perform any lookups directly as the bookkeeping for
           variable types is done by Inferno. We ignore the return
           value of [instance] since we are working with a monomorphic
           type scheme. *)
        let+ _ = instance id r in
        (* We can ignore the return value of [instance] since it will always
             return the empty list since all local variables are bound with
             [def], which creates a monomorphic scheme. *)
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
          | Some t -> deep (pty_to_deep t)
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
let typecheck c = Solver.solve ~rectypes:false c

let process_fun_spec f =
  Option.fold ~some:(fun _ -> assert false) ~none:(pure None) f

let ty_of_pty = Types.Tyapp (S.bool_id, [])

(** [function_cstr ts f cstr] Creates a constraint whose semantic value is a
    list of signatures whose head is the declaration of the function described
    by [f]. This is the conjunction of three constraints: one that adds the
    function we are processing to the scope of [cstr], another that checks if
    the body of the function is well typed and another that checks if the
    function's specification is well typed. *)
let function_cstr (f : Id_uast.function_)
    (cstr : (s_signature * s_signature list) co) :
    (s_signature_item_desc * s_signature * s_signature list) co =
  (* Turn the return type into a deep type *)
  let deep_ret = Option.fold ~some:pty_to_deep ~none:deep_bool f.fun_type in
  let@ ret_ty = deep deep_ret in

  (* Map each type annotation of a parameter to a deep type. *)
  let to_deep (arg, pty) =
    let deep_arg = pty_to_deep pty in
    (arg, deep_arg)
  in
  let deep_params = List.map to_deep f.fun_params in

  (* The function type encoded as a deep type *)
  let deep_fun_type =
    List.fold_right
      (fun (_, arg) ret -> deep_arrow arg ret)
      deep_params deep_ret
  in
  let@ fun_ty = deep deep_fun_type in

  (* This constraint adds the function to the scope of the constraint
     [cstr] and has the same semantic value. In other words, it
     produces the list of typed signatures that have been
     processed up this point. *)
  let c1 = def f.fun_name fun_ty cstr in

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

  (* List of typed signatures*)
  let+ l, stack = c1
  (* Typed term and function parameters *)
  and+ params, tt = build_def deep_params body_c
  (* Typed function specification *)
  and+ fun_spec = process_fun_spec f.fun_spec in
  let fun_ty = Option.fold ~some:(fun t -> t.t_ty) ~none:ty_of_pty tt in
  (Sig_function (mk_function f params tt fun_ty fun_spec), l, stack)

(** Creates a constraint ensuring the term within an axiom has type [bool]. *)
let axiom_cstr ax =
  let@ ty = shallow S.ty_bool in
  let+ t = hastype ax.Id_uast.ax_term ty in
  Sig_axiom (mk_axiom ax.ax_name t ax.ax_loc ax.ax_text)

(** Auxiliary function for signatures that do not define any symbols in the top
    level (which is all except [Sig_function]) *)
let rec sig_rest cstr s =
  let+ s =
    match s.Id_uast.sdesc with
    | Sig_axiom ax -> axiom_cstr ax
    | _ -> assert false
  and+ l, stack = cstr in
  (s, l, stack)

(** [module_cstr] creates a constraint whose semantic value is a list of
    signatures whose head is the declaration of [m]. Since the work to make the
    Gospel specification compatible with Inferno (which does not support
    modules) has been done at this point (see module [Inferno_prep]), the
    constraint we create assumes that the definitions in this module are in the
    same scope as the ones defined in [cstr].*)
and module_cstr (m : Id_uast.s_module_declaration) cstr =
  let mdname = m.mdname in
  let mdloc = m.mdloc in
  let mdattributes = m.mdattributes in
  let mtype = m.mdtype in
  let mloc = mtype.mloc in
  let mattributes = mtype.mattributes in

  let+ mdesc, l, stack =
    match mtype.mdesc with
    | Id_uast.Mod_signature s ->
        let cstr =
          (* Since we need to build the typed submodule, we need to clear the
             current list of definitions from the semantic value of the
             constraint so that the call to [signature] returns only the
             definitions within the submodule. Naturally, we cannot simply drop
             the list of definitions as we will need these later to build the
             module we were previously processing, so we add it to the stack and
             pop it once we are done. *)
          let+ l, stack = cstr in
          ([], l :: stack)
        in
        let+ l, stack = signature s cstr in
        (* The call to [List.hd] and [List.tl] reset list of definitions and the
           stack to the state, respectively, before processing the submodule. *)
        (Mod_signature l, List.hd stack, List.tl stack)
    | _ -> assert false
  in
  let mdtype = { mdesc; mloc; mattributes } in
  (Sig_module { mdname; mdloc; mdattributes; mdtype }, l, stack)

(** [signature ts cstr s] Transforms the signature [s] into an typed signature.
    Since signatures are handled from the bottom up, [cstr] represents all the
    constraints we have already built up to this point. The sematic value of
    [cstr] is the list of signatures that have already been processed coupled
    with a list of lists of signatures. The latter list contain lists of
    definitions of parent modules of the module we are currently processing.
    Since this list does not concern the current call of [signature_item], it is
    ignored.

    The returned constraint will either be [cstr] unchanged or series of [def]
    constraints where we define either a function or a predicate (or multiple in
    case of a module). Its semantic value will be the semantic value of
    constraint of [cstr] where the first list is modified by adding the typed
    signature to its head. The second list is unchanged. *)
and signature_item s cstr =
  let+ sdesc, l, stack =
    match s.Id_uast.sdesc with
    | Sig_function f -> function_cstr f cstr
    | Sig_module m -> module_cstr m cstr
    | _ -> sig_rest cstr s
  in
  ({ sdesc; sloc = s.sloc } :: l, stack)

and signature l cstr = List.fold_right signature_item l cstr

let signatures l =
  let l = Typing.signatures l in
  (* Build a constraint for the entire Gospel file and solve it. *)
  let c = signature l (pure ([], [])) in
  let loc loc = Uast_utils.mk_loc loc in
  let error r = W.error ~loc:(loc r) in
  try typecheck (let0 c) with
  | Solver.Unbound _ ->
      assert
        false (* Unbound variables are caught in the [Inferno_prep] module *)
  | Solver.Unify (r, ty1, ty2) ->
      let ty1s = Fmt.str "%a" Types.print_ty ty1 in
      let ty2s = Fmt.str "%a" Types.print_ty ty2 in
      error r (Bad_type (ty1s, ty2s))
  | _ -> assert false
