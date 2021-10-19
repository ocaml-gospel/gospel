open Tterm
open Ttypes
open Utils
module Ident = Identifier.Ident

let lsymbol ?(constr = false) ~field ls_name ls_args ls_value =
  { ls_name; ls_args; ls_value; ls_constr = constr; ls_field = field }

let fsymbol ?(constr = false) ~field nm tyl ty =
  lsymbol ~constr ~field nm tyl (Some ty)

let psymbol nm ty = lsymbol ~field:false nm ty None

let ls_subst_ts old_ts new_ts ({ ls_name; ls_constr; ls_field; _ } as ls) =
  let ls_args = List.map (ty_subst_ts old_ts new_ts) ls.ls_args in
  let ls_value = Option.map (ty_subst_ts old_ts new_ts) ls.ls_value in
  lsymbol ls_name ls_args ls_value ~constr:ls_constr ~field:ls_field

let ls_subst_ty old_ts new_ts new_ty ls =
  let subst ty = ty_subst_ty old_ts new_ts new_ty ty in
  let ls_args = List.map subst ls.ls_args in
  let ls_value = Option.map subst ls.ls_value in
  lsymbol ls.ls_name ls_args ls_value ~constr:ls.ls_constr ~field:ls.ls_field

(** buil-in lsymbols *)

let ps_equ =
  let tv = fresh_ty_var "a" in
  psymbol Identifier.eq [ tv; tv ]

let fs_unit = fsymbol ~constr:true ~field:false (Ident.create "unit") [] ty_unit

let fs_bool_true =
  fsymbol ~constr:true ~field:false (Ident.create "True") [] ty_bool

let fs_bool_false =
  fsymbol ~constr:true ~field:false (Ident.create "False") [] ty_bool

let fs_apply =
  let ty_a, ty_b = (fresh_ty_var "a", fresh_ty_var "b") in
  let ty_a_to_b = ty_app ts_arrow [ ty_a; ty_b ] in
  fsymbol ~field:false (Ident.create "apply") [ ty_a_to_b; ty_a ] ty_b

(* CHECK do we need two hash tables? *)
let fs_tuple_ids = Hashtbl.create 17

let fs_tuple =
  let ls_tuples = Hashtbl.create 17 in
  fun n ->
    try Hashtbl.find ls_tuples n
    with Not_found ->
      let id = Ident.create ("tuple" ^ string_of_int n) in
      let tyl = List.init n (fun _ -> fresh_ty_var "a") in
      let ty = ty_app (ts_tuple n) tyl in
      let ls = fsymbol ~constr:true ~field:false id tyl ty in
      Hashtbl.add fs_tuple_ids id ls;
      Hashtbl.add ls_tuples n ls;
      ls

let is_fs_tuple fs = fs.ls_constr = true && Hashtbl.mem fs_tuple_ids fs.ls_name

let rec p_vars p =
  match p.p_node with
  | Pwild -> Svs.empty
  | Pvar vs -> Svs.singleton vs
  | Papp (_, pl) ->
      List.fold_left (fun vsl p -> Svs.union (p_vars p) vsl) Svs.empty pl
  | Por (p1, p2) -> Svs.union (p_vars p1) (p_vars p2)
  | Pas (p, vs) -> Svs.add vs (p_vars p)

let rec t_free_vars t =
  match t.t_node with
  | Tvar vs -> Svs.singleton vs
  | Tconst _ -> Svs.empty
  | Tapp (_, tl) ->
      List.fold_left (fun fvs t -> Svs.union (t_free_vars t) fvs) Svs.empty tl
  | Tfield (t, _) -> t_free_vars t
  | Tif (t1, t2, t3) ->
      Svs.union (t_free_vars t1) (Svs.union (t_free_vars t2) (t_free_vars t3))
  | Tlet (vs, t1, t2) ->
      let t1_fvs, t2_fvs = (t_free_vars t1, t_free_vars t2) in
      Svs.union t1_fvs (Svs.remove vs t2_fvs)
  | Tcase (t, pl) ->
      let t_fvs = t_free_vars t in
      let pl_fvs =
        List.fold_left
          (fun _ (p, t) -> Svs.diff (t_free_vars t) (p_vars p))
          Svs.empty pl
      in
      Svs.union t_fvs pl_fvs
  | Tquant (_, vl, t) -> Svs.diff (t_free_vars t) (Svs.of_list vl)
  | Tbinop (_, t1, t2) -> Svs.union (t_free_vars t1) (t_free_vars t2)
  | Tnot t -> t_free_vars t
  | Told t -> t_free_vars t
  | Ttrue -> Svs.empty
  | Tfalse -> Svs.empty

exception FreeVariables of Svs.t

let t_free_vs_in_set svs t =
  let diff = Svs.diff (t_free_vars t) svs in
  check ~loc:t.t_loc (Svs.is_empty diff) (FreeVariables diff)

(** type checking *)

exception TermExpected of term
exception FmlaExpected of term

let t_prop t = if t.t_ty = None then t else error ~loc:t.t_loc (FmlaExpected t)

let t_type t =
  match t.t_ty with
  | Some ty -> ty
  | None -> error ~loc:t.t_loc (TermExpected t)

let t_ty_check t ty =
  match (ty, t.t_ty) with
  | Some l, Some r -> ty_equal_check l r
  | Some _, None -> raise (TermExpected t)
  | None, Some _ -> raise (FmlaExpected t)
  | None, None -> ()

exception BadArity of lsymbol * int
exception PredicateSymbolExpected of lsymbol
exception FunctionSymbolExpected of lsymbol

let ls_arg_inst ls tl =
  try
    List.fold_left2
      (fun tvm ty t -> ty_match tvm ty (t_type t))
      Mtv.empty ls.ls_args tl
  with Invalid_argument _ ->
    let loc = (List.hd tl).t_loc in
    error ~loc (BadArity (ls, List.length tl))

let ls_app_inst ls tl ty =
  let s = ls_arg_inst ls tl in
  match (ls.ls_value, ty) with
  | Some _, None -> raise (PredicateSymbolExpected ls)
  | None, Some _ -> raise (FunctionSymbolExpected ls)
  | Some vty, Some ty -> ty_match s vty ty
  | None, None -> s

(** Pattern constructors *)

let mk_pattern p_node p_ty p_vars = { p_node; p_ty; p_vars; p_loc = None }

exception PDuplicatedVar of vsymbol
exception EmptyCase

let p_wild ty = mk_pattern Pwild ty Svs.empty
let p_var vs = mk_pattern (Pvar vs) vs.vs_ty (Svs.singleton vs)

let p_app ls pl ty =
  let add v vars =
    if Svs.mem v vars then raise (PDuplicatedVar v);
    Svs.add v vars
  in
  let merge vars p = Svs.fold add vars p.p_vars in
  let vars = List.fold_left merge Svs.empty pl in
  mk_pattern (Papp (ls, pl)) ty vars

(* CHECK ty matchs ls.ls_value *)
let p_or p1 p2 = mk_pattern (Por (p1, p2)) p1.p_ty p1.p_vars

(* CHECK vars p1 = vars p2 *)
let p_as p vs = mk_pattern (Pas (p, vs)) p.p_ty p.p_vars
(* CHECK type vs = type p *)

(** Terms constructors *)

let mk_term t_node t_ty t_loc = { t_node; t_ty; t_attrs = []; t_loc }
let t_var vs = mk_term (Tvar vs) (Some vs.vs_ty)
let t_const c ty = mk_term (Tconst c) (Some ty)

let t_app ls tl ty =
  ignore (ls_app_inst ls tl ty : ty Mtv.t);
  mk_term (Tapp (ls, tl)) ty

let t_field t ls ty =
  ignore (ls_app_inst ls [ t ] ty : ty Mtv.t);
  mk_term (Tfield (t, ls)) ty

let t_if t1 t2 t3 = mk_term (Tif (t1, t2, t3)) t2.t_ty
let t_let vs t1 t2 = mk_term (Tlet (vs, t1, t2)) t2.t_ty

let t_case t1 ptl =
  match ptl with
  | [] -> error ~loc:t1.t_loc EmptyCase
  | (_, t) :: _ -> mk_term (Tcase (t1, ptl)) t.t_ty

let t_quant q vsl t ty = mk_term (Tquant (q, vsl, t)) ty
let t_binop b t1 t2 = mk_term (Tbinop (b, t1, t2)) None
let t_not t = mk_term (Tnot t) None
let t_old t = mk_term (Told t) t.t_ty
let t_true = mk_term Ttrue None
let t_false = mk_term Tfalse None
let t_attr_set attr t = { t with t_attrs = attr }
let t_bool_true = mk_term (Tapp (fs_bool_true, [])) (Some ty_bool)
let t_bool_false = mk_term (Tapp (fs_bool_false, [])) (Some ty_bool)
let t_equ t1 t2 = t_app ps_equ [ t1; t2 ] None
let t_neq t1 t2 loc = t_not (t_equ t1 t2 loc)

(* smart-constructors with type checking *)

let f_binop op f1 f2 = t_binop op (t_prop f1) (t_prop f2)
let f_not f = t_not (t_prop f)

let t_quant q vsl t ty loc =
  match (q, vsl) with
  | Tlambda, [] -> t
  | _, [] -> t_prop t
  | Tlambda, _ -> t_quant q vsl t ty loc
  | _, _ ->
      check_report (ty = None) "Quantifiers terms must be of type prop.";
      t_quant q vsl (t_prop t) None loc

let f_forall = t_quant Tforall
let f_exists = t_quant Texists
let t_lambda = t_quant Tlambda
let f_and = f_binop Tand
let f_and_asym = f_binop Tand_asym
let f_or = f_binop Tor
let f_or_asym = f_binop Tor_asym
let f_implies = f_binop Timplies
let f_iff = f_binop Tiff
