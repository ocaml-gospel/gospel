(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

open Utils
open Identifier
open Ttypes
open Tterm

(* types *)

type dty =
  | Tvar of dtvar
  | Tapp of tysymbol * dty list
  | Tty  of ty

and dtvar = {
    dtv_id          : int;
    mutable dtv_def : dty option;
}

let dty_fresh =
  let i = ref 0 in
  fun () -> incr i;
            Tvar {dtv_id = !i; dtv_def = None}

let dty_of_ty ty = Tty ty

(* Maybe this does not neet to be global. It can be declared inside
   the function term

   We definitively need a map between the dtv_id and identifiers (or
   tyvars)
*)
(* let tyvars = Hashtbl.create 17 *)

let ty_of_dty =
  let tyvars = Hashtbl.create 0 in
  fun dty ->
    let get_var id = try Hashtbl.find tyvars id with Not_found ->
      let ty = fresh_ty_var ("a" ^ string_of_int id)  in
      Hashtbl.add tyvars id ty; ty in
    let rec to_ty dty = match dty with
      | Tvar {dtv_id;dtv_def = None} -> get_var dtv_id
      | Tvar {dtv_def = Some dty} -> to_ty dty
      | Tapp (ts,dtyl) -> ty_app  ts (List.map to_ty dtyl)
      | Tty ty -> ty in
    to_ty dty

(* predefined types *)

let dty_integer = dty_of_ty ty_integer
let dty_bool    = dty_of_ty ty_bool
let dty_float   = dty_of_ty ty_float
let dty_char    = dty_of_ty ty_char
let dty_string  = dty_of_ty ty_string

(* lsymbol specialization *)

let specialize_ls ls =
  let htv = Htv.create 3 in
  let find_tv tv =
    try Htv.find htv tv with Not_found ->
      let dtv = dty_fresh () in Htv.add htv tv dtv; dtv in
  let rec spec ty = match ty.ty_node with
    | Tyvar tv -> find_tv tv
    | Tyapp (ts,tyl) -> Tapp (ts,List.map spec tyl) in
  List.map spec ls.ls_args, Option.map spec ls.ls_value

exception ConstructorExpected of lsymbol

let specialize_cs ?loc cs =
  if cs.ls_constr = false then error ?loc (ConstructorExpected cs);
  let dtyl, dty = specialize_ls cs in
  dtyl, Option.get dty

(* terms *)

module Mstr = Map.Make(String)

type dpattern = {
  dp_node : dpattern_node;
  dp_dty  : dty;
  dp_vars : dty Mstr.t;
  dp_loc  : Location.t option;
}

and dpattern_node =
  | DPwild
  | DPvar of Preid.t
  | DPapp of lsymbol * dpattern list
  | DPor of dpattern * dpattern
  | DPas of dpattern * Preid.t
  | DPcast of dpattern * dty

type dbinder = Preid.t * dty

type dterm = {
  dt_node  : dterm_node;
  dt_dty   : dty option;
  dt_loc   : Location.t option;
}

and dterm_node =
  (*
   * | Trecord of (Preid.t * term) list
   * | Tupdate of term * (Preid.t * term) list
   *)
  | DTattr  of dterm * string list
  | DTvar   of Preid.t
  | DTconst of Oasttypes.constant
  | DTapp   of lsymbol * dterm list
  | DTif    of dterm * dterm * dterm
  | DTlet   of Preid.t * dterm * dterm
  | DTcase  of dterm * (dpattern * dterm) list
  | DTquant of quant * dbinder list * dterm list list * dterm
  | DTbinop of binop * dterm * dterm
  | DTnot   of dterm
  | DTold   of dterm
  | DTtrue
  | DTfalse

let dty_of_dterm dt = match dt.dt_dty with
  | None -> dty_bool
  | Some dty -> dty

(* type unification *)

let rec head = function
  | Tvar {dtv_def=None; _} as t -> t
  | Tvar {dtv_def=Some t; _} -> head t
  | dty -> dty

(* TODO review the use of this function. Not sure if it is a good idea
   to receive an index *)
let rec occur i dty = match head dty with
  | Tty _ -> false
  | Tvar {dtv_id; _} -> i = dtv_id
  | Tapp (_,dtys) ->
     try List.iter (fun x -> if occur i x then raise Exit) dtys; false
     with Exit -> true

let rec unify_dty_ty dty ty = match head dty, ty.ty_node with
  | Tvar tvar, _ -> tvar.dtv_def <- Some (Tty ty)
  | Tty ty1, _ when ty_equal ty1 ty -> ()
  | Tapp (ts1,dl), Tyapp (ts2,tl) when ts_equal ts1 ts2 -> begin
      try List.iter2 unify_dty_ty dl tl with
        Invalid_argument _ -> raise Exit end
  | _ -> raise Exit

let rec unify dty1 dty2 = match head dty1, head dty2 with
  | Tvar {dtv_id=id1}, Tvar {dtv_id=id2} when id1 = id2 -> ()
  | Tvar tvar, dty | dty, Tvar tvar ->
     if occur tvar.dtv_id dty then raise Exit else
       tvar.dtv_def <- Some dty
  | Tapp (ts1,dtyl1), Tapp (ts2,dtyl2) when ts_equal ts1 ts2 -> begin
      try List.iter2 unify dtyl1 dtyl2 with
        Invalid_argument _ -> raise Exit end
  | Tty ty, dty | dty, Tty ty -> unify_dty_ty dty ty
  | _ -> raise Exit

exception PatternBadType of dty * dty
exception BadType of dty * dty
exception FmlaExpected

(* CHECK isn't it better to use only BadType? *)
exception TermExpected

(* the following functions should receive dterms for precise locations
   to be given properly -- based on why3 *)

let app_unify ls unify l dtyl2 =
  try List.iter2 unify l dtyl2 with Invalid_argument _ ->
    raise (BadArity (ls, List.length l))

let app_unify_map ls unify l dtyl =
  try List.map2 unify l dtyl with Invalid_argument _ ->
    raise (BadArity (ls, List.length l))

let dpattern_unify dp dty =
  try unify dp.dp_dty dty with Exit ->
    error ?loc:(dp.dp_loc) (PatternBadType (dp.dp_dty, dty))

let dty_unify ?loc dty1 dty2 =
  try unify dty1 dty2 with Exit ->
    error ?loc (BadType (dty1, dty2))

let dterm_unify dt dty = match dt.dt_dty with
  | Some dt_dty -> dty_unify ?loc:dt.dt_loc dt_dty dty
  | None -> begin try unify dty_bool dty with Exit ->
                    error ?loc:dt.dt_loc TermExpected end

let dfmla_unify dt = match dt.dt_dty with
  | None -> ()
  | Some dt_dty -> begin try unify dt_dty dty_bool with Exit ->
                           error ?loc:dt.dt_loc FmlaExpected end

let unify dt dty = match dty with
  | None -> dfmla_unify dt
  | Some dt_dty -> dterm_unify dt dt_dty

(* environment *)

type denv = dty Mstr.t

exception DuplicatedVar of string
exception UnboundVar of string

let denv_find ?loc s denv =
  try Mstr.find s denv with
    Not_found -> error ?loc (UnboundVar s)

let is_in_denv denv s = Mstr.mem s denv

let denv_empty = Mstr.empty

let denv_get_opt denv s = Mstr.find_opt s denv

let denv_add_var denv s dty = Mstr.add s dty denv

let denv_add_var_quant denv vl =
  let add acc (pid,dty) =
    if Mstr.mem pid.Preid.pid_str acc
    then error ~loc:pid.pid_loc (DuplicatedVar pid.pid_str)
    else Mstr.add pid.pid_str dty acc  in
  let vl = List.fold_left add Mstr.empty vl in
  let choose_snd _ _ x = Some x in
  Mstr.union choose_snd denv vl

(** coercions *)

let apply_coercion l ({dt_loc = loc} as dt) =
  let apply dt ls =
    let dtyl, dty = specialize_ls ls in
    dterm_unify dt (List.hd dtyl);
    { dt_node = DTapp (ls, [dt]); dt_dty = dty; dt_loc = loc } in
  List.fold_left apply dt l


(* coercions using just head tysymbols without type arguments: *)
(* TODO: this can be improved *)
let rec ts_of_dty = function
  | Tvar {dtv_def = Some dty} ->
      ts_of_dty dty
  | Tvar {dtv_def = None}
  | Tty {ty_node = Tyvar _} ->
      raise Exit
  | Tty {ty_node = Tyapp (ts,_)}
  | Tapp (ts,_) ->
      ts

let ts_of_dty = function
  | Some dt_dty -> ts_of_dty dt_dty
  | None        -> ts_bool

(* NB: this function is not a morphism w.r.t.
   the identity of type variables. *)
let rec ty_of_dty_raw = function
  | Tvar { dtv_def = Some (Tty ty) } ->
     ty
  | Tvar ({ dtv_def = Some dty }) ->
     ty_of_dty_raw dty
  | Tvar _ ->
     fresh_ty_var "xi"
  | Tapp (ts,dl) ->
     ty_app ts (List.map (ty_of_dty_raw) dl)
  | Tty ty -> ty

let ty_of_dty_raw = function
  | Some dt_dty -> ty_of_dty_raw dt_dty
  | None        -> ty_bool

let max_dty crcmap dtl =
  let rec aux = function
    | (dty1, ts1, ty1) :: l ->
        (* find a type that cannot be coerced to another type *)
        let check (_, ts2, ty2) =
          try
            if not (ts_equal ts1 ts2) then
              ignore (Coercion.find crcmap ty1 ty2);
            true
          with Not_found -> false in
        (* by transitivity, we never have to look back *)
        if List.exists check l then aux l else dty1
    | [] -> assert false in
  let l = List.fold_left (fun acc { dt_dty = dty } ->
    try (dty, ts_of_dty dty, ty_of_dty_raw dty) :: acc
    with Exit -> acc) [] dtl in
  if l == [] then (List.hd dtl).dt_dty
  else aux l

let max_dty crcmap dtl =
  match max_dty crcmap dtl with
  | Some (Tty ty)
    when ty_equal ty ty_bool
    && List.exists (fun { dt_dty = dty } -> dty = None) dtl ->
      (* favor prop over bool *)
      None
  | dty -> dty

let dterm_expected crcmap dt dty =
  try
    let (ts1, ts2) = ts_of_dty dt.dt_dty, ts_of_dty dty in
    if (ts_equal ts1 ts2) then dt
    else
      let (ty1, ty2) = ty_of_dty_raw dt.dt_dty, ty_of_dty_raw dty in
      let crc = Coercion.find crcmap ty1 ty2 in
      apply_coercion crc dt
  with Not_found | Exit -> dt

let dterm_expected_op crcmap dt dty =
  let dt = dterm_expected crcmap dt dty in
  unify dt dty;
  dt

let dfmla_expected crcmap dt =
  dterm_expected_op crcmap dt None

let dterm_expected crcmap dt dty =
  dterm_expected_op crcmap dt (Some dty)

(** dterm to tterm *)

let pattern dp =
  let vars = ref Mstr.empty in
  let get_var pid ty =
    try Mstr.find pid.Preid.pid_str !vars with Not_found ->
      let vs = create_vsymbol pid ty in (* TODO the variable found is of type ty *)
      vars := Mstr.add pid.pid_str vs !vars; vs in
  let rec pattern_node dp =
    let ty = ty_of_dty dp.dp_dty in
    match dp.dp_node with
    | DPwild -> p_wild ty
    | DPvar pid -> p_var (get_var pid ty)
    | DPapp (ls,dpl) ->
       p_app ls (List.map pattern_node dpl) ty
    | DPor (dp1,dp2) ->
       let dp1 = pattern_node dp1 in
       let dp2 = pattern_node dp2 in
       p_or dp1 dp2
    | DPas (dp,pid) -> p_as (pattern_node dp) (get_var pid ty)
    | DPcast _ -> assert false
  in
  let p = pattern_node dp in
  p, !vars

let rec term env prop dt =
  let t = term_node ?loc:dt.dt_loc env prop dt.dt_dty dt.dt_node in
  match t.t_ty with
  | Some _ when prop -> begin try t_equ t t_bool_true with
      TypeMismatch (ty1,ty2) ->
        error ?loc:dt.dt_loc (BadType (dty_of_ty ty1, dty_of_ty ty2)) end
  | None when not prop -> t_if t t_bool_true t_bool_false
  | _ -> t

and term_node ?loc env prop dty dterm_node =
  match dterm_node with
  | DTvar pid ->
     let vs = denv_find ~loc:pid.pid_loc pid.pid_str env in (* TODO should I match vs.vs_ty with dty? *)
     t_var vs
  | DTconst c ->
     t_const c (ty_of_dty (Option.get dty))
  | DTapp (ls,[]) when ls_equal ls fs_bool_true ->
     if prop then t_true else t_bool_true
  | DTapp (ls,[]) when ls_equal ls fs_bool_false ->
     if prop then t_false else t_bool_false
  | DTapp (ls,[dt1;dt2]) when ls_equal ls ps_equ ->
     if dt1.dt_dty = None || dt2.dt_dty = None then
       f_iff (term env true dt1) (term env true dt2)
     else t_equ (term env false dt1) (term env false dt2)
  | DTapp (ls,dtl) ->
     t_app ls (List.map (term env false) dtl) (Option.map ty_of_dty dty)
  | DTif (dt1,dt2,dt3) ->
     let prop = prop || dty = None in
     t_if (term env true dt1) (term env prop dt2) (term env prop dt3)
  | DTlet (pid,dt1,dt2) ->
     let prop = prop || dty = None in
     let t1 = term env false dt1 in
     let vs = create_vsymbol pid (t_type t1) in
     let env = Mstr.add pid.pid_str vs env in
     let t2 = term env prop dt2 in
     t_let vs t1 t2
  | DTbinop (b,dt1,dt2) ->
     let t1, t2 = term env true dt1, term env true dt2 in
     t_binop b t1 t2
  | DTnot dt -> t_not (term env true dt)
  | DTtrue -> if prop then t_true else t_bool_true
  | DTfalse -> if prop then t_false else t_bool_false
  | DTattr (dt,at) ->
     let t = term env prop dt in
     t_attr_set ?loc at t
  | DTold dt -> t_old (term env prop dt)
  | DTquant (q,bl,dtl,dt) ->
     let add_var (env,vsl) (pid,dty) =
       let vs = create_vsymbol pid (ty_of_dty dty) in
       Mstr.add pid.pid_str vs env, vs::vsl in
     let env,vsl = List.fold_left add_var (env,[]) bl in
     (* CHECK not sure if we want prop in triggers *)
     let tl = List.map (List.map (term env false)) dtl in
     let t = term env prop dt in
     t_quant q (List.rev vsl) tl t (Option.map ty_of_dty dty)
  | DTcase (dt,ptl) ->
     let t = term env false dt in
     let branch (dp,dt) =
       let p,vars = pattern dp in
       let join _ _ vs = Some vs in
       let env = Mstr.union join env vars in
       p, term env false dt in
     let pl = List.map branch ptl in
     t_case t pl

let fmla env dt =
  term env true dt

let term env dt =
  term env false dt

(* Pretty printing *)

open Opprintast
open Fmt

(* TODO not sure if we need this. Maybe we just need this for pretty
   print dterm. The flatten part is already done in ty_of_dty. *)
let rec flatten = function
  | Tvar {dtv_def=Some dty; _} -> flatten dty
  | Tapp (ts,dtys) -> Tapp (ts, List.map flatten dtys)
  | dty -> dty

let rec print_dty fmt dty = match flatten dty with
  | Tty ty -> print_ty fmt ty
  | Tvar {dtv_id;dtv_def=None} -> pp fmt "@%d@" dtv_id
  | Tvar {dtv_def=Some _; _} -> assert false (* it is flattened *)
  | Tapp (ts,dtyl) -> match dtyl with
     | [] -> print_ts_name fmt ts
     | dtyl when is_ts_arrow ts ->
        pp fmt "(%a)" (list ~sep:arrow print_dty) dtyl
     | [dty] ->
        pp fmt "%a %a" print_dty dty print_ts_name ts
     | dtyl ->
        pp fmt "(%a) %a" (list ~sep:comma print_dty) dtyl print_ts_name ts

let rec print_dpattern fmt {dp_node;dp_dty} = match dp_node with
  | DPwild -> pp fmt "_"
  | DPvar pid -> pp fmt "%a:%a" Preid.pp pid print_dty dp_dty
  | DPapp (ls,dpl) when is_fs_tuple ls ->
     pp fmt "(%a)" (list ~sep:comma print_dpattern) dpl

  | DPapp (ls,dpl) ->
     pp fmt "%a %a" print_ls_nm ls
       (list ~sep:sp print_dpattern) dpl
  | DPor (dp1,dp2) ->
     pp fmt "(%a | %a):%a" print_dpattern dp1 print_dty dp_dty
       print_dpattern dp2
  | DPas (dp,pid) ->
     pp fmt "(%a as %a):%a" print_dpattern dp
       Preid.pp pid print_dty dp_dty
  | DPcast (dp,dty) ->
     pp fmt "(%a:%a):%a" print_dpattern dp print_dty dty
       print_dty dp_dty

let rec print_dterm fmt {dt_node; dt_dty; _} =
  let print_dty fmt dty = match dty with
      None -> ()
    | Some dty -> pp fmt ":%a" print_dty dty in
  match dt_node with
  | DTconst c -> pp fmt "%a%a" constant c print_dty dt_dty
  | DTtrue -> pp fmt "true%a" print_dty dt_dty
  | DTfalse -> pp fmt "false%a" print_dty dt_dty
  | DTvar v -> pp fmt "%a%a" Preid.pp v print_dty dt_dty
  | DTapp (ls,dtl) -> pp fmt "(%a %a)%a"
                        Ident.pp ls.ls_name
                        (list ~sep:sp print_dterm) dtl
                        print_dty dt_dty
  | DTnot t -> pp fmt "not %a" print_dterm t
  | DTif (t1,t2,t3) -> pp fmt "if %a then %a else %a" print_dterm t1
                         print_dterm t2 print_dterm t3
  | DTlet (pid,t1,t2) -> pp fmt "let %a = %a in %a" Preid.pp pid
                          print_dterm t1 print_dterm t2
  | DTbinop (op,t1,t2) -> pp fmt "%a %a %a" print_binop op
                            print_dterm t1 print_dterm t2
  | DTquant (q,vl,trl,dt) ->
     let print_quant_v fmt (pid,dty) =
       pp fmt "%a%a" Preid.pp pid print_dty (Some dty) in
     pp fmt "%a %a %a. %a"
       print_quantifier q
       (list ~sep:sp print_quant_v) vl
       (fun _ _ -> ()) trl
       print_dterm dt
  | DTcase (dt, dptl) ->
     let print_branch fmt (dp,dt) =
       pp fmt "| %a -> %a" print_dpattern dp print_dterm dt in
     pp fmt "match %a with@\n%a@\nend:%a"
       print_dterm dt
       (list ~sep:newline print_branch) dptl
       print_dty dt_dty
  | _ -> assert false

let () =
  let open Location in
  register_error_of_exn (function
      | ConstructorExpected ls ->
         Some (errorf "Symbol %a is not a constructor" print_ls_nm ls)
      | FmlaExpected ->
         Some (errorf "Formula was expected")
      | TermExpected -> Some (errorf "Term was expected")
      | PatternBadType (dty1,dty2) ->
         Some (errorf "This pattern has type %a but is expected to \
                       have type %a" print_dty dty2 print_dty dty1)
      | BadType (dty1,dty2) ->
         Some (errorf "Type mysmatch. Cannot match %a with %a"
                 print_dty dty1 print_dty dty2)
      | DuplicatedVar s ->
         Some (errorf "Variable %s is duplicated in pattern" s)
      | UnboundVar s ->
         Some (errorf "Variable %s does not appear in pattern" s)
      | _ -> None)
