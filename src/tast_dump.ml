open Fmt
open Utils
open Ppxlib
open Tast
module Ident = Identifier.Ident

let loc f _loc = pf f "XXX todo dump location"
let string_list f l = pf f "@[[ %a ]@]" (list ~sep:comma string) l
let tvsymbol f (tv : Ttypes.tvsymbol) = pf f "%s" tv.tv_name.id_str
let tvsymbol_list f tvs = pf f "@[[ %a ]@]" (list ~sep:comma tvsymbol) tvs

let rec ty f (t : Ttypes.ty) = pf f "{ @[ty_node: %a@] }" ty_node t.ty_node

and ty_node f = function
  | Ttypes.Tyvar tv -> pf f "Ttypes.Tyvar @[%a@]" tvsymbol tv
  | Ttypes.Tyapp (ts, tys) ->
      pf f "@[Ttypes.Tyapp@\n  @[(@[%a@],@\n@[%a@])@]@]" tysymbol ts ty_list tys

and tysymbol f (ts : Ttypes.tysymbol) =
  pf f "@[{@\n  @[ts_ident: %s;@\nts_args: %a@\nts_alias: %a@\n@]}@]"
    ts.ts_ident.id_str tvsymbol_list ts.ts_args ty_option ts.ts_alias

and vsymbol f (vs : Tterm.vsymbol) =
  pf f "@[{@\n  @[vs_name: %s;@\nvs_ty: %a@]@\n }@]" vs.vs_name.id_str ty
    vs.vs_ty

and ty_list f tys = pf f "@[[ %a ]@]" (list ~sep:comma ty) tys
and ty_option f = function None -> pf f "None" | Some t -> pf f "Some %a" ty t

let lb_arg f (lb : lb_arg) =
  match lb with
  | Lunit -> pf f "Lunit"
  | Lnone vs -> pf f "Lnone %a" vsymbol vs
  | Loptional vs -> pf f "Loptional %a" vsymbol vs
  | Lnamed vs -> pf f "Lnamed %a" vsymbol vs
  | Lghost vs -> pf f "Lghost %a" vsymbol vs

let lb_args f args = pf f "@[[ %a ]@]" (list ~sep:comma lb_arg) args

let lsymbol f (ls : Tterm.lsymbol) =
  pf f
    "@[{@\n\
    \  @[ls_name: %s;@\n\
     ls_args: %a@\n\
     ls_value: %a@\n\
     ls_constr: %B;@\n\
     ls_field: %B @]@\n\
     }@]"
    ls.ls_name.id_str ty_list ls.ls_args ty_option ls.ls_value ls.ls_constr
    ls.ls_field

let rec term_node f (tn : Tterm.term_node) =
  match tn with
  | Tterm.Tvar vs -> pf f "Tterm.Tvar %a" vsymbol vs
  | Tterm.Tconst _ -> pf f "XXX todo dump Tterm.Tconst"
  | Tterm.Tapp (ls, ts) ->
      pf f "Tterm.Tapp@\n  @[(%a, %a)@]" lsymbol ls terms ts
  | Tterm.Tfield (t, ls) -> pf f "Tterm.Tfield (%a, %a)" term t lsymbol ls
  | Tterm.Tif (cond, t0, t1) ->
      pf f "Tterm.Tif (%a, %a, %a)" term cond term t0 term t1
  | Tterm.Tlet (vs, t0, t1) ->
      pf f "Tterm.Tlet (%a, %a, %a)" vsymbol vs term t0 term t1
  | Tterm.Tcase (_, _) -> pf f "XXX todo dump Tterm.Tcase"
  | Tterm.Tquant (_, _, _) -> pf f "XXX todo dump Tterm.Tquant"
  | Tterm.Tbinop (_, _, _) -> pf f "XXX todo dump Tterm.binop"
  | Tterm.Tnot t -> pf f "Tterm.not %a" term t
  | Tterm.Told t -> pf f "Tterm.old %a" term t
  | Tterm.Ttrue -> pf f "Tterm.Ttrue"
  | Tterm.Tfalse -> pf f "Tterm.Tfalse"

and term f (t : Tterm.term) =
  pf f "@[{@\n  @[t_node: %a;@\nt_ty: %a;@\nt_attrs: %a;@\nt_loc: %a@\n@]}@]"
    term_node t.t_node ty_option t.t_ty string_list t.t_attrs loc t.t_loc

and terms f ts = pf f "@[[ %a ]@]" (list ~sep:comma term) ts

let xpost f _ = pf f "todo"
let xposts f xp = pf f "@[[ %a ]@]" (list ~sep:comma xpost) xp

let val_spec f (spec : val_spec option) =
  match spec with
  | None -> string f "None"
  | Some vspec ->
      pf f
        "{@\n\
        \  @[sp_args: %a;@\n\
         sp_ret: %a;@\n\
         sp_pre: %a;@\n\
         sp_checks: %a;@\n\
         sp_post: %a;@\n\
         sp_xpost: %a;@\n\
         sp_wr: %a;@\n\
         sp_cs: %a;@\n\
         sp_diverge: %B;@\n\
         sp_pure: %B;@\n\
         sp_equiv: %a;@\n\
         sp_text: @[%s@];@\n\
         sp_loc: %a;@\n\
         @]}"
        lb_args vspec.sp_args lb_args vspec.sp_ret terms vspec.sp_pre terms
        vspec.sp_checks terms vspec.sp_post xposts vspec.sp_xpost terms
        vspec.sp_wr terms vspec.sp_cs vspec.sp_diverge vspec.sp_pure string_list
        vspec.sp_equiv vspec.sp_text loc vspec.sp_loc

let attributes f (_attrs : Parsetree.attributes) =
  pf f "XXX todo dump attributes"

let core_type f (_t : Parsetree.core_type) = pf f "XXX todo dump core_type"
let ident f (i : Ident.t) = pf f "%s" i.id_str

let val_description f (vd : val_description) =
  pf f
    "{@\n\
    \  vd_name: %a;@\n\
    \  vd_type: %a;@\n\
    \  vd_prim: %a;@\n\
    \  vd_attrs: %a;@\n\
    \  vd_args: %a;@\n\
    \  vd_ret: %a;@\n\
    \  vd_spec: @[%a;@]@\n\
    \  vd_loc: %a@\n\
     }@\n"
    ident vd.vd_name core_type vd.vd_type (list ~sep:comma string) vd.vd_prim
    attributes vd.vd_attrs lb_args vd.vd_args lb_args vd.vd_ret val_spec
    vd.vd_spec loc vd.vd_loc

let signature_item f (s : signature_item) =
  match s.sig_desc with
  | Sig_type (_, _td, _g) -> pf f "XXX todo dump Sig_type"
  | Sig_val (vd, g) -> pf f "Sig_val (@[%a@], %B)@\n" val_description vd g
  | Sig_typext _te -> pf f "XXX todo dump Sig_typext"
  | Sig_exception _ed -> pf f "XXX todo dump Sig_exception"
  | Sig_class _l -> pf f "XXX todo dump Sig_class"
  | Sig_module _pmd -> pf f "XXX todo dump Sig_module"
  | Sig_open (_od, _ghost) -> pf f "XXX todo dump Sig_open"
  | Sig_include _incl -> pf f "XXX todo dump Sig_include"
  | Sig_modtype { mtd_name = _s; mtd_type = _md; mtd_attrs = _attrs } ->
      pf f "XXX todo dump Sig_modtype"
  | Sig_class_type _l -> pf f "XXX todo dump Sig_class"
  | Sig_extension (_e, _a) -> pf f "XXX todo dump Sig_extension"
  | Sig_function _x -> pf f "XXX todo dump Sig_function"
  | Sig_axiom _x -> pf f "XXX todo dump Sig_axiom"
  | Sig_use _s -> pf f "XXX todo dump Sig_use"
  | _ -> assert false

let signature f x = list ~sep:Fmt.newline signature_item f x
