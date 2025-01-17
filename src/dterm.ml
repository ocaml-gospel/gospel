(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

module W = Warnings
open Ppxlib
open Utils
open Identifier
open Ttypes
open Symbols
open Tterm
open Tterm_helper

(* types *)

type dty = Tvar of dtvar | Tapp of tysymbol * dty list | Tty of ty
and dtvar = { dtv_id : int; mutable dtv_def : dty option }

let dty_fresh =
  let i = ref 0 in
  fun () ->
    incr i;
    Tvar { dtv_id = !i; dtv_def = None }

let dty_of_ty ty = Tty ty

let ty_of_dty =
  let tyvars = Hashtbl.create 0 in
  fun dty ->
    let get_var id =
      try Hashtbl.find tyvars id
      with Not_found ->
        let ty = fresh_ty_var ~loc:Location.none ("a" ^ string_of_int id) in
        Hashtbl.add tyvars id ty;
        ty
    in
    let rec to_ty dty =
      match dty with
      | Tvar { dtv_id; dtv_def = None } -> get_var dtv_id
      | Tvar { dtv_def = Some dty; _ } -> to_ty dty
      | Tapp (ts, dtyl) -> ty_app ts (List.map to_ty dtyl)
      | Tty ty -> ty
    in
    to_ty dty

(* predefined types *)

let dty_integer = dty_of_ty ty_integer
let dty_int = dty_of_ty ty_int
let dty_bool = dty_of_ty ty_bool
let dty_float = dty_of_ty ty_float
let dty_char = dty_of_ty ty_char
let dty_string = dty_of_ty ty_string

(* lsymbol specialization *)

let specialize_ls ls =
  let htv = Htv.create 3 in
  let find_tv tv =
    try Htv.find htv tv
    with Not_found ->
      let dtv = dty_fresh () in
      Htv.add htv tv dtv;
      dtv
  in
  let rec spec ty =
    match ty.ty_node with
    | Tyvar tv -> find_tv tv
    | Tyapp (ts, tyl) -> Tapp (ts, List.map spec tyl)
  in
  (List.map spec (get_args ls), spec (get_value ls))

let specialize_cs ~loc cs =
  match cs with
  | Constructor_symbol _ ->
      let dtyl, dty = specialize_ls cs in
      (dtyl, dty)
  | Field_symbol { ls_name; _ } | Function_symbol { ls_name; _ } ->
      W.error ~loc (W.Not_a_constructor ls_name.id_str)

(* terms *)

module Mstr = Map.Make (String)

type dpattern = {
  dp_node : dpattern_node;
  dp_dty : dty;
  dp_vars : dty Mstr.t;
  dp_loc : Location.t;
}

and dpattern_node =
  | DPwild
  | DPvar of Preid.t
  | DPapp of lsymbol * dpattern list
  | DPor of dpattern * dpattern
  | DPas of dpattern * Preid.t
  | DPcast of dpattern * dty
  | DPconst of Parsetree.constant
  | DPinterval of char * char

type dbinder = Preid.t * dty

type dterm = { dt_node : dterm_node; dt_dty : dty option; dt_loc : Location.t }

and dterm_node =
  | DTattr of dterm * string list
  | DTvar of Preid.t
  | DTconst of constant
  | DTapp of lsymbol * dterm list
  | DTif of dterm * dterm * dterm
  | DTlet of Preid.t * dterm * dterm
  | DTcase of dterm * (dpattern * dterm option * dterm) list
  | DTquant of quant * dbinder list * dterm
  | DTlambda of dpattern list * dterm
  | DTbinop of binop * dterm * dterm
  | DTnot of dterm
  | DTold of dterm
  | DTtrue
  | DTfalse

let dty_of_dterm dt = match dt.dt_dty with None -> dty_bool | Some dty -> dty

(* type unification *)

let rec head = function
  | Tvar { dtv_def = None; _ } as t -> t
  | Tvar { dtv_def = Some t; _ } -> head t
  | dty -> dty

let rec occur dtvar dty =
  match head dty with
  | Tty _ -> false
  | Tvar { dtv_id; _ } -> dtvar.dtv_id = dtv_id
  | Tapp (_, dtys) -> List.exists (occur dtvar) dtys

let rec unify_dty_ty dty ty =
  match (head dty, ty.ty_node) with
  | Tvar tvar, _ -> tvar.dtv_def <- Some (Tty ty)
  | Tty ty1, _ when ty_equal ty1 ty -> ()
  | Tapp (ts1, dl), Tyapp (ts2, tl) when ts_equal ts1 ts2 ->
      List.iter2 unify_dty_ty dl tl
  | _ -> raise Exit

let rec unify dty1 dty2 =
  match (head dty1, head dty2) with
  | Tvar { dtv_id = id1; _ }, Tvar { dtv_id = id2; _ } when id1 = id2 -> ()
  | Tvar tvar, dty | dty, Tvar tvar ->
      if occur tvar dty then raise Exit else tvar.dtv_def <- Some dty
  | Tapp (ts1, dtyl1), Tapp (ts2, dtyl2) when ts_equal ts1 ts2 ->
      List.iter2 unify dtyl1 dtyl2
  | Tty ty, dty | dty, Tty ty -> unify_dty_ty dty ty
  | _ -> raise Exit

(* the following functions should receive dterms for precise locations
   to be given properly -- based on why3 *)

let app_unify ~loc ls unify l dtyl2 =
  if List.length l <> List.length dtyl2 then
    W.error ~loc
      (W.Bad_arity
         ((get_name ls).id_str, List.length (get_args ls), List.length l));
  List.iter2 unify l dtyl2

let app_unify_map ~loc ls unify l dtyl =
  if List.length l <> List.length dtyl then
    W.error ~loc
      (W.Bad_arity
         ((get_name ls).id_str, List.length (get_args ls), List.length l));
  List.map2 unify l dtyl

let dpattern_unify dp dty =
  try unify dp.dp_dty dty
  with Exit ->
    let t1 = Fmt.str "%a" print_ty (ty_of_dty dp.dp_dty) in
    let t2 = Fmt.str "%a" print_ty (ty_of_dty dty) in
    W.error ~loc:dp.dp_loc (W.Pattern_bad_type (t1, t2))

let dty_unify ~loc dty1 dty2 =
  let t1 = Fmt.str "%a" print_ty (ty_of_dty dty1) in
  let t2 = Fmt.str "%a" print_ty (ty_of_dty dty2) in
  try unify dty1 dty2 with Exit -> W.error ~loc (W.Bad_type (t1, t2))

let dterm_unify dt dty =
  match dt.dt_dty with
  | Some dt_dty -> dty_unify ~loc:dt.dt_loc dt_dty dty
  | None -> assert false

let dfmla_unify dt =
  match dt.dt_dty with
  | None -> assert false
  | Some dt_dty -> (
      try unify dt_dty dty_bool
      with Exit -> W.error ~loc:dt.dt_loc W.Formula_expected)

let unify dt dty =
  match dty with None -> assert false | Some dt_dty -> dterm_unify dt dt_dty

(* environment *)

type denv = dty Mstr.t

let denv_find ~loc s denv =
  try Mstr.find s denv with Not_found -> W.error ~loc (W.Unbound_variable s)

let is_in_denv denv s = Mstr.mem s denv

(* let denv_empty = Mstr.empty *)
let denv_get_opt denv s = Mstr.find_opt s denv
let denv_add_var denv s dty = Mstr.add s dty denv

let denv_add_var_quant denv vl =
  let add acc (pid, dty) =
    if Mstr.mem pid.Preid.pid_str acc then
      W.error ~loc:pid.pid_loc (W.Duplicated_variable pid.pid_str)
    else Mstr.add pid.pid_str dty acc
  in
  let vl = List.fold_left add Mstr.empty vl in
  let choose_snd _ _ x = Some x in
  Mstr.union choose_snd denv vl

(** coercions *)

(* coercions using just head tysymbols without type arguments: *)
(* TODO: this can be improved *)
let rec ts_of_dty = function
  | Tvar { dtv_def = Some dty; _ } -> ts_of_dty dty
  | Tvar { dtv_def = None; _ } | Tty { ty_node = Tyvar _ } -> raise Exit
  | Tty { ty_node = Tyapp (ts, _) } | Tapp (ts, _) -> ts

let ts_of_dty = function Some dt_dty -> ts_of_dty dt_dty | None -> ts_bool

(* NB: this function is not a morphism w.r.t.
   the identity of type variables. *)
let rec ty_of_dty_raw = function
  | Tvar { dtv_def = Some (Tty ty); _ } -> ty
  | Tvar { dtv_def = Some dty; _ } -> ty_of_dty_raw dty
  | Tvar _ -> fresh_ty_var ~loc:Location.none "xi"
  | Tapp (ts, dl) -> ty_app ts (List.map ty_of_dty_raw dl)
  | Tty ty -> ty

let ty_of_dty_raw = function
  | Some dt_dty -> ty_of_dty_raw dt_dty
  | None -> ty_bool

let max_dty dtl =
  let rec aux = function
    | (dty1, ts1, _ty1) :: l ->
        (* find a type that cannot be coerced to another type *)
        let check (_, ts2, _ty2) = ts_equal ts1 ts2 in
        (* by transitivity, we never have to look back *)
        if List.exists check l then aux l else dty1
    | [] -> assert false
  in
  let l =
    List.fold_left
      (fun acc { dt_dty; _ } ->
        try (dt_dty, ts_of_dty dt_dty, ty_of_dty_raw dt_dty) :: acc
        with Exit -> acc)
      [] dtl
  in
  if l = [] then (List.hd dtl).dt_dty else aux l

let dterm_expected_op dt dty =
  unify dt dty;
  dt

let dfmla_expected dt = dterm_expected_op dt (Some dty_bool)
let dterm_expected dt dty = dterm_expected_op dt (Some dty)

(** dterm to tterm *)

let pattern dp =
  let vars = ref Mstr.empty in
  let get_var pid ty =
    try Mstr.find pid.Preid.pid_str !vars
    with Not_found ->
      let vs = create_vsymbol pid ty in
      (* TODO the variable found is of type ty *)
      vars := Mstr.add pid.pid_str vs !vars;
      vs
  in
  let rec pattern_node dp =
    let ty = ty_of_dty dp.dp_dty and loc = dp.dp_loc in
    match dp.dp_node with
    | DPwild -> p_wild ty loc
    | DPvar pid -> p_var (get_var pid ty) loc
    | DPconst c -> p_const c loc
    | DPapp (ls, dpl) -> p_app ls (List.map pattern_node dpl) ty loc
    | DPor (dp1, dp2) ->
        let dp1 = pattern_node dp1 in
        let dp2 = pattern_node dp2 in
        p_or dp1 dp2 loc
    | DPas (dp, pid) -> p_as (pattern_node dp) (get_var pid ty) loc
    | DPinterval (c1, c2) -> p_interval c1 c2 loc
    | DPcast (dp, _) -> pattern_node dp
  in
  let p = pattern_node dp in
  (p, !vars)

let rec term env dt =
  let loc = dt.dt_loc in
  term_node ~loc env dt.dt_dty dt.dt_node

and term_node ~loc env dty dterm_node =
  match dterm_node with
  | DTvar pid ->
      let vs = denv_find ~loc:pid.pid_loc pid.pid_str env in
      (* TODO should I match vs.vs_ty with dty? *)
      t_var vs loc
  | DTconst c -> t_const c (ty_of_dty (Option.get dty)) loc
  | DTapp (ls, []) when ls_equal ls fs_bool_true -> t_true loc
  | DTapp (ls, []) when ls_equal ls fs_bool_false -> t_false loc
  | DTapp ((Field_symbol _ as ls), [ dt1 ]) ->
      t_field (term env dt1) ls
        (Option.fold ~some:ty_of_dty ~none:ty_bool dty)
        loc
  | DTapp (ls, dtl) ->
      t_app ls
        (List.map (term env) dtl)
        (Option.fold ~some:ty_of_dty ~none:ty_bool dty)
        loc
  | DTif (dt1, dt2, dt3) ->
      t_if (term env dt1) (term env dt2) (term env dt3) loc
  | DTlet (pid, dt1, dt2) ->
      let t1 = term env dt1 in
      let vs = create_vsymbol pid t1.t_ty in
      let env = Mstr.add pid.pid_str vs env in
      let t2 = term env dt2 in
      t_let vs t1 t2 loc
  | DTbinop (b, dt1, dt2) ->
      let t1, t2 = (term env dt1, term env dt2) in
      t_binop b t1 t2 loc
  | DTnot dt -> t_not (term env dt) loc
  | DTtrue -> t_true loc
  | DTfalse -> t_false loc
  | DTattr (dt, at) ->
      let t = term env dt in
      t_attr_set at t
  | DTold dt -> t_old (term env dt) loc
  | DTquant (q, bl, dt) ->
      let add_var (env, vsl) (pid, dty) =
        let vs = create_vsymbol pid (ty_of_dty dty) in
        (Mstr.add pid.pid_str vs env, vs :: vsl)
      in
      let env, vsl = List.fold_left add_var (env, []) bl in
      let t = term env dt in
      t_quant q (List.rev vsl) t (ty_of_dty (Option.get dty)) loc
  | DTlambda (dpl, dt) ->
      let ty = ty_of_dty_raw dty and pl = List.map pattern dpl in
      let env =
        let join _ _ vs = Some vs in
        List.fold_left (fun env (_, vs) -> Mstr.union join env vs) env pl
      in
      let t = term env dt in
      (* Are the patterns exhaustive? *)
      List.iter
        (fun (p, _) ->
          let loc = p.p_loc in
          Patmat.checks p.p_ty
            [ (p, None, t (* [t] is really just a place holder *)) ]
            ~loc)
        pl;
      t_lambda (List.map fst pl) t ty loc
  | DTcase (dt, ptl) ->
      let t = term env dt in
      let branch (dp, guard, dt) =
        let p, vars = pattern dp in
        let join _ _ vs = Some vs in
        let env = Mstr.union join env vars in
        let dt = term env dt in
        let guard =
          match guard with None -> None | Some g -> Some (term env g)
        in
        (p, guard, dt)
      in
      let pl = List.map branch ptl in
      let ty = ty_of_dty (Option.get dt.dt_dty) in
      Patmat.checks ty pl ~loc;
      t_case t pl loc

let term env dt = term env dt
