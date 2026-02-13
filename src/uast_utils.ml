(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)
open Parse_uast

let mk_term t l = { term_desc = t; term_loc = l }

(** [mk_op_apply op arg1 arg2] Creates a term for the application of the
    operator [op] to the list of arguments [args]. Although this function could
    be used for any function application, the locations of the subterms would
    not correspond to what would be expected. *)
let mk_op_apply op args =
  let f = mk_term (Tvar (Qid op)) op.pid_loc in
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
          let mk_pid =
            Preid.create ~fixity:Preid.Infix ~attrs:[] ~loc:Location.none
          in
          let qconj = mk_pid "/\\" in
          let infix = mk_term (mk_op_apply o [ t1; t3 ]) t.term_loc in
          mk_term (mk_op_apply qconj [ infix; t ]) Location.none
      | _ ->
          (* If the right subtree is not another infix operation, we
            translate the term naturally*)
          mk_term (mk_op_apply o [ t1; t2 ]) t.term_loc)
  | _ -> t

(** [args_to_list t] returns two lists, the first contains the types of
    arguments that values of type [t] receive (empty if [t] is not an arrow
    type). The second contains a singleton list with the return type. If [t] is
    a tuple [t1 * ... * tn], returns the list [t1; ...; tn]. *)
let rec args_to_list t =
  match t with
  | Id_uast.PTarrow (t1, t2) ->
      let args, ret = args_to_list t2 in
      (t1 :: args, ret)
  | PTtuple l -> ([], l)
  | _ -> ([], [ t ])

(** [create_header nm nargs nrets] creates a [spec_header] with [nargs]
    arguments and a single return value, all of which will be [Lwild]. *)
let create_header nm nargs =
  let mk_arg _ = Lwild in
  let args = List.init nargs mk_arg in
  let rets = if nargs = 0 then [ Lvar nm ] else [ Lwild ] in
  { sp_hd_nm = nm; sp_hd_args = args; sp_hd_ret = rets }

(** Let operator that ignores [None] values *)
let ( let* ) o f = match o with None -> None | Some x -> Some (f x)

(** Used to chain multiple [let*]. *)
let ( and* ) x y =
  match (x, y) with None, _ | _, None -> None | Some x, Some y -> Some (x, y)

let rec map_option f = function
  | [] -> Some []
  | x :: t ->
      let* x = f x and* t = map_option f t in
      x :: t

let get_name = function
  | Lunit _ | Lwild -> None
  | Lvar id | Lghost (id, _) -> Some id

let v_eq v1 v2 =
  let v_eq v1 v2 =
    let* v1 = get_name v1 and* v2 = get_name v2 in
    Preid.eq v1 v2
  in
  Option.value ~default:false (v_eq v1 v2)

(** Same behaviour as [chain] but receives and retruns a [term_desc] *)
let chain_desc t = (chain { term_desc = t; term_loc = Location.none }).term_desc

(** Location smart constructor *)
let mk_loc (s, e) =
  { Location.loc_start = s; Location.loc_end = e; Location.loc_ghost = false }

let flatten q =
  let rec flatten = function
    | Qid id -> [ id.pid_str ]
    | Qdot (q, id) -> id.pid_str :: flatten q
  in
  List.rev (flatten q)

let leaf q = match q with Id_uast.Qid id -> id | Qdot (_, id) -> id

let flatten_ident q =
  let rec flatten = function
    | Id_uast.Qid id -> [ id.id_str ]
    | Qdot (q, id) -> id.id_str :: flatten q
  in
  List.rev (flatten q)

(** [eq_qualid q1 q2] Checks if the qualified identifier [q1] and [q2] refer to
    the same identifier. This function ignores if the two identifiers have
    different symbolic representations. For example: [M1.x] is the same as [x],
    assuming that both identifiers refer to the same variable. *)
let eq_qualid q1 q2 =
  let id1 = leaf q1 in
  let id2 = leaf q2 in
  Ident.equal id1 id2

(** [ocaml_to_model defs ty] Turns an OCaml type into its logical
    representation. This entails searching in the [defs] environment for the
    logical representation of the type names used in [ty]. If any of the type
    names used in [ty] do not have a Gospel model (or if [ty] includes arrow
    types), this function returns [None]. *)
let rec ocaml_to_model ty =
  let open Id_uast in
  match ty with
  | PTtyvar v ->
      (* Note: The treatment of type variables within the Gospel type
        checker is still unclear: currently we assume that we can use
        an OCaml type variable as if it were a Gospel type variable,
        which is unsound since OCaml types may be impure and
        therefore unusable in a logical context. *)
      Some (PTtyvar v)
  | PTtyapp (q, _) -> q.app_model
  | PTtuple l ->
      let* l = map_option ocaml_to_model l in
      PTtuple l
  | PTarrow (arg, ret) ->
      let* arg = ocaml_to_model arg and* ret = ocaml_to_model ret in
      PTarrow (arg, ret)

(** [can_own ty] traverses an OCaml type and checks if one needs to claim
    ownership of this value in order to use it within a specification.. *)
let rec can_own ty =
  match ty with
  | Id_uast.PTtyvar _ -> false
  | PTtyapp (id, l) -> id.app_mut || List.exists can_own l
  | PTtuple l -> List.exists can_own l
  | PTarrow _ ->
      (* For now, we assume that all OCaml functions are pure. *) false

let qualid_loc = function Id_uast.Qid id | Qdot (_, id) -> id.id_loc
