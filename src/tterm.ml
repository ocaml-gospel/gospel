(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

open Ttypes
open Utils

module Ident = Identifier.Ident

(* Variable Symbols *)

type vsymbol = {
  vs_name : Ident.t;
  vs_ty   : ty;
}

let create_vsymbol pid ty = {
    vs_name = Ident.of_preid pid;
    vs_ty = ty;
}

module Vs = struct
  type t = vsymbol
  let compare = Stdlib.compare
end

module Svs = Set.Make(Vs)

(* Function and predicate symbols *)

type lsymbol = {
  ls_name   : Ident.t;
  ls_args   : ty list;
  ls_value  : ty option;
  ls_constr : bool; (* true if it is a construct, false otherwise*)
}

(* CHECK *)
let ls_equal : lsymbol -> lsymbol -> bool = (==)

module LS = struct
  type t = lsymbol
  let compare = Stdlib.compare
  let equal = ls_equal
  let hash = (Hashtbl.hash : lsymbol -> int)
end

module Sls = Set.Make(LS)
module Mls = Map.Make(LS)


let lsymbol ?(constr=false) ls_name ls_args ls_value =
  {ls_name;ls_args;ls_value;ls_constr=constr}

let fsymbol ?(constr=false) nm tyl ty =
  lsymbol ~constr nm tyl (Some ty)

let psymbol nm ty =
  lsymbol nm ty None

let ls_subst_ts old_ts new_ts ({ls_name;ls_constr;_} as ls) =
  let ls_args = List.map (ty_subst_ts old_ts new_ts) ls.ls_args in
  let ls_value = Option.map (ty_subst_ts old_ts new_ts) ls.ls_value in
  lsymbol ls_name ls_args ls_value ~constr:ls_constr

let ls_subst_ty old_ts new_ts new_ty ls =
  let subst ty = ty_subst_ty old_ts new_ts new_ty ty in
  let ls_args = List.map subst ls.ls_args in
  let ls_value = Option.map subst ls.ls_value in
  lsymbol ls.ls_name ls_args ls_value ~constr:ls.ls_constr

(** buil-in lsymbols *)

let ps_equ =
  let tv = fresh_ty_var "a" in
  psymbol Identifier.eq [tv; tv]

let fs_unit = fsymbol ~constr:true (Ident.create "unit") [] ty_unit

let fs_bool_true  = fsymbol ~constr:true (Ident.create "True")  [] ty_bool
let fs_bool_false = fsymbol ~constr:true (Ident.create "False") [] ty_bool

let fs_apply =
  let ty_a, ty_b = fresh_ty_var "a", fresh_ty_var "b" in
  let ty_a_to_b = ty_app ts_arrow [ty_a;ty_b] in
  fsymbol (Ident.create "apply") [ty_a_to_b; ty_a] ty_b

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
      let ls = fsymbol  ~constr:true id tyl ty in
      Hashtbl.add fs_tuple_ids id ls;
      Hashtbl.add ls_tuples n ls; ls

let is_fs_tuple fs =
  fs.ls_constr = true && Hashtbl.mem fs_tuple_ids fs.ls_name

(** terms *)

type pattern = {
  p_node : pattern_node;
  p_ty   : ty;
  p_vars : Svs.t;
  p_loc  : Location.t option;
}

and pattern_node =
  | Pwild
  | Pvar of vsymbol
  | Papp of lsymbol * pattern list
  | Por of pattern * pattern
  | Pas of pattern * vsymbol

type binop = Tand | Tand_asym | Tor | Tor_asym | Timplies | Tiff
  (* TODO: think about 'by' and 'so' *)

type quant = Tforall | Texists | Tlambda

type term = {
  t_node  : term_node;
  t_ty    : ty option;
  t_attrs : string list;
  t_loc   : Location.t option;
}

and term_node =
  | Tvar   of vsymbol
  | Tconst of Oasttypes.constant
  | Tapp   of lsymbol * term list
  | Tif    of term * term * term
  | Tlet   of vsymbol * term * term
  | Tcase  of term * (pattern * term) list
  | Tquant of quant * vsymbol list * trigger * term
  | Tbinop of binop * term * term
  | Tnot   of term
  | Told   of term
  | Ttrue
  | Tfalse

and trigger = term list list

let rec p_vars p = match p.p_node with
  | Pwild -> Svs.empty
  | Pvar vs -> Svs.singleton vs
  | Papp (_,pl) -> List.fold_left (fun vsl p ->
      Svs.union (p_vars p) vsl) Svs.empty pl
  | Por (p1,p2) -> Svs.union (p_vars p1) (p_vars p2)
  | Pas (p,vs) -> Svs.add vs (p_vars p)

let rec t_free_vars t = match t.t_node with
  | Tvar vs -> Svs.singleton vs
  | Tconst _ -> Svs.empty
  | Tapp (_,tl) -> List.fold_left (fun fvs t ->
      Svs.union (t_free_vars t) fvs) Svs.empty tl
  | Tif (t1,t2,t3) -> Svs.union (t_free_vars t1)
      (Svs.union (t_free_vars t2) (t_free_vars t3))
  | Tlet (vs,t1,t2) ->
     let t1_fvs, t2_fvs = t_free_vars t1,  t_free_vars t2 in
     Svs.union t1_fvs (Svs.remove vs t2_fvs)
  | Tcase (t,pl) ->
     let t_fvs = t_free_vars t in
     let pl_fvs = List.fold_left (fun _ (p,t) ->
       Svs.diff (t_free_vars t) (p_vars p)) Svs.empty pl in
     Svs.union t_fvs pl_fvs
  | Tquant (_,vl,tr,t) ->
     let vars acc t = Svs.union (t_free_vars t) acc in
     let vars = List.fold_left (fun acc tl ->
       List.fold_left vars acc tl) (t_free_vars t) tr in
     Svs.diff vars (Svs.of_list vl)
  | Tbinop (_,t1,t2) -> Svs.union (t_free_vars t1) (t_free_vars t2)
  | Tnot t -> t_free_vars t
  | Told t -> t_free_vars t
  | Ttrue -> Svs.empty
  | Tfalse -> Svs.empty

exception FreeVariables of Svs.t

let t_free_vs_in_set svs t =
  let diff = Svs.diff (t_free_vars t) svs in
  check ?loc:t.t_loc (Svs.is_empty diff) (FreeVariables diff)

(** type checking *)

exception TermExpected of term
exception FmlaExpected of term

let t_prop t =
  if t.t_ty = None then t else raise (FmlaExpected t)

let t_type t = match t.t_ty with
  | Some ty -> ty
  | None -> raise (TermExpected t)

let t_ty_check t ty = match ty, t.t_ty with
  | Some l, Some r -> ty_equal_check l r
  | Some _, None -> raise (TermExpected t)
  | None, Some _ -> raise (FmlaExpected t)
  | None, None -> ()

(** smart-constructors for terms *)

exception BadArity of lsymbol * int
exception PredicateSymbolExpected of lsymbol
exception FunctionSymbolExpected of lsymbol


let mk_term n ty = {
    t_node  = n;
    t_ty    = ty;
    t_attrs = [];
    t_loc   = None;
}

(* TODO: missing locations when reporting errors *)
let ls_arg_inst ls tl =
  try List.fold_left2 (fun tvm ty t ->
        ty_match tvm ty (t_type t)) Mtv.empty ls.ls_args tl
  with Invalid_argument _ -> raise (BadArity (ls,List.length tl))

let ls_app_inst ls tl ty =
  let s = ls_arg_inst ls tl in
  match ls.ls_value, ty with
    | Some _, None -> raise (PredicateSymbolExpected ls)
    | None, Some _ -> raise (FunctionSymbolExpected ls)
    | Some vty, Some ty -> ty_match s vty ty
    | None, None -> s

let mk_pattern pn ty vl = {
    p_node = pn;
    p_ty = ty;
    p_vars = vl;
    p_loc = None;
  }

exception PDuplicatedVar of vsymbol
exception EmptyCase

let p_wild ty          = mk_pattern Pwild ty Svs.empty
let p_var vs           = mk_pattern (Pvar vs) vs.vs_ty (Svs.singleton vs)
let p_app ls pl ty     =
  let add v vars =
    if Svs.mem v vars then raise (PDuplicatedVar v); Svs.add v vars in
  let merge vars p = Svs.fold add vars p.p_vars in
  let vars = List.fold_left merge Svs.empty  pl in
  mk_pattern (Papp (ls,pl)) ty vars
  (* CHECK ty matchs ls.ls_value *)
let p_or p1 p2         = mk_pattern (Por (p1,p2)) p1.p_ty p1.p_vars
(* CHECK vars p1 = vars p2 *)
let p_as p vs          =

  mk_pattern (Pas (p,vs)) p.p_ty p.p_vars
(* CHECK type vs = type p *)

let t_var vs           = mk_term (Tvar vs) (Some vs.vs_ty)
let t_const c ty       = mk_term (Tconst c) (Some ty)
let t_app ls tl ty     = ignore(ls_app_inst ls tl ty);
                         mk_term (Tapp (ls,tl)) ty
let t_if t1 t2 t3      = mk_term (Tif (t1,t2,t3)) t2.t_ty
let t_let vs t1 t2     = mk_term (Tlet (vs,t1,t2)) t2.t_ty
let t_case t1 ptl      = match ptl with
  | [] -> error ?loc:t1.t_loc EmptyCase
  | (_,t) :: _ -> mk_term (Tcase (t1,ptl)) t.t_ty
let t_quant q vsl tr t ty = mk_term (Tquant (q,vsl,tr,t)) ty
let t_binop b t1 t2    = mk_term (Tbinop (b,t1,t2)) None
let t_not t            = mk_term (Tnot t) None
let t_old t            = mk_term (Told t) t.t_ty
let t_true             = mk_term (Ttrue) None
let t_false            = mk_term (Tfalse) None

let t_attr_set ?loc attr t =
  let t_loc = if loc = None then t.t_loc else loc in
  {t with t_attrs = attr; t_loc}

let t_bool_true        = mk_term (Tapp (fs_bool_true, [])) (Some ty_bool)
let t_bool_false       = mk_term (Tapp (fs_bool_false,[])) (Some ty_bool)

let t_equ t1 t2 = t_app ps_equ [t1; t2] None
let t_neq t1 t2 = t_not (t_equ t1 t2)

(* smart-constructors with type checking *)

let f_binop op f1 f2  = t_binop op (t_prop f1) (t_prop f2)
let f_not f = t_not (t_prop f)

let t_quant q vsl tr t ty = match q,vsl with
  | Tlambda, [] -> t
  | _, []       -> t_prop t
  | Tlambda, _  -> t_quant q vsl tr t ty
  | _, _        ->
     check_report (ty = None) "Quantifiers terms must be of type prop.";
     t_quant q vsl tr (t_prop t) None

let f_forall = t_quant Tforall
let f_exists = t_quant Texists
let t_lambda = t_quant Tlambda

let f_and      = f_binop Tand
let f_and_asym = f_binop Tand_asym
let f_or       = f_binop Tor
let f_or_asym  = f_binop Tor_asym
let f_implies  = f_binop Timplies
let f_iff      = f_binop Tiff

(** Pretty printing *)

open Opprintast
open Fmt

let print_vs fmt {vs_name; vs_ty} =
  pp fmt "@[%a:%a@]" Ident.pp vs_name print_ty vs_ty

let print_ls_decl fmt {ls_name;ls_args;ls_value} =
  let is_func = Option.is_some ls_value in
  let print_unnamed_arg fmt ty = pp fmt "(_:%a)" print_ty ty in
  pp fmt "%s %a %a%s%a"
    (if is_func then "function" else "predicate")
    Ident.pp ls_name
    (list ~sep:sp print_unnamed_arg) ls_args
    (if is_func then " : " else "")
    (Fmt.option print_ty) ls_value

let print_ls_nm fmt {ls_name} =
  pp fmt "%a" Ident.pp ls_name

let protect_on x s = if x then "(" ^^ s ^^ ")" else s

let rec print_pat_node pri fmt p = match p.p_node with
  | Pwild ->
      pp fmt "_"
  | Pvar v ->
      print_vs fmt v
  | Pas (p, v) ->
      pp fmt (protect_on (pri > 1) "%a as %a")
        (print_pat_node 1) p print_vs v
  | Por (p, q) ->
      pp fmt (protect_on (pri > 0) "%a | %a")
        (print_pat_node 0) p (print_pat_node 0) q
  | Papp (cs, pl) when is_fs_tuple cs ->
      pp fmt (protect_on (pri > 0) "%a")
        (list ~sep:comma (print_pat_node 1)) pl
  | Papp (cs, []) ->
      print_ls_nm fmt cs
  | Papp (cs, pl) ->
      pp fmt (protect_on (pri > 1) "%a@ %a")
        print_ls_nm cs (list ~sep:sp (print_pat_node 2)) pl

let print_pattern = print_pat_node 0

let print_binop fmt = function
  | Tand -> pp fmt "/\\"
  | Tor -> pp fmt "\\/"
  | Timplies -> pp fmt "->"
  | Tiff -> pp fmt "<->"
  | Tand_asym ->  pp fmt "&&"
  | Tor_asym -> pp fmt "||"


let print_quantifier fmt = function
  | Tforall -> pp fmt "forall"
  | Texists -> pp fmt "exists"
  | Tlambda -> pp fmt "fun"

(* TODO use pretty printer from why3 *)
let rec print_term fmt {t_node; t_ty; t_attrs; _ } =
  let print_ty fmt ty = match ty with
      None -> pp fmt ":prop"
    | Some ty -> pp fmt ":%a" print_ty ty in
  let print_t_node fmt t_node = match t_node with
    | Tconst c -> pp fmt "%a%a" constant c print_ty t_ty
    | Ttrue -> pp fmt "true%a" print_ty t_ty
    | Tfalse -> pp fmt "false%a" print_ty t_ty
    | Tvar vs ->
       pp fmt "%a" print_vs vs;
       assert (vs.vs_ty = Option.get t_ty ) (* TODO remove this *)
    | Tapp (ls,[x1;x2]) when Identifier.is_infix ls.ls_name.id_str ->
       let op_nm =
         match String.split_on_char ' ' ls.ls_name.id_str with
           | [x] | [_; x] -> x
           | _ -> assert false
       in
       pp fmt "(%a %s %a)%a"
         print_term x1
         op_nm
         print_term x2
         print_ty t_ty
    | Tapp (ls,tl) ->
       pp fmt "(%a %a)%a"
         Ident.pp ls.ls_name
         (list ~first:sp ~sep:sp print_term) tl
         print_ty t_ty
    | Tnot t -> pp fmt "not %a" print_term t
    | Tif (t1,t2,t3) ->
       pp fmt "if %a then %a else %a" print_term t1
         print_term t2 print_term t3
    | Tlet (vs,t1,t2) ->
       pp fmt "let %a = %a in %a" print_vs vs
         print_term t1 print_term t2
    | Tbinop (op,t1,t2) ->
       pp fmt "%a %a %a" print_term t1
         print_binop op print_term t2
    | Tquant (q,vsl,trl,t) ->
       pp fmt "%a %a %a. %a"
         print_quantifier q
         (list ~sep:sp print_vs) vsl
         (fun _ _ -> ()) trl
         print_term t
    | Tcase (t, ptl) ->
       let print_branch fmt (p,t) =
         pp fmt "| @[%a@] -> @[%a@]" print_pattern p print_term t in
       pp fmt "match %a with@\n%a@\nend:%a"
         print_term t
         (list ~sep:newline print_branch) ptl
         print_ty t_ty
    | Told t ->
       pp fmt "old (%a)" print_term t
  in
  let print_attrs fmt = List.iter (pp fmt "[%@ %s]") in
  pp fmt "%a%a" print_attrs t_attrs print_t_node t_node

(** register exceptions *)

let () =
  let open Location in
  register_error_of_exn (function
      | TermExpected t ->
         Some (errorf "Term expected in %a" print_term t)
      | FmlaExpected t ->
         Some (errorf "Formula expected in %a" print_term t)
      | BadArity (ls,i) ->
         Some (errorf "Function %a expects %d arguments as opposed to %d"
                 print_ls_nm ls (List.length ls.ls_args) i)
      | PredicateSymbolExpected ls ->
         Some (errorf "Not a predicate symbol: %a" print_ls_nm ls)
      | FunctionSymbolExpected ls ->
         Some (errorf "Not a function symbol: %a" print_ls_nm ls)
      | FreeVariables svs ->
         Some (errorf "Unbound variables: %a" (list ~sep:comma print_vs)
                 (Svs.elements svs) )
      | _ -> None)
