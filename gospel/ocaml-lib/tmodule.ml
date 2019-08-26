open Utils
open Identifier
open Ttypes
open Tterm
open Tast

(** Namespace *)

type namespace = {
    ns_ts  : tysymbol  Mstr.t;
    ns_ls  : lsymbol   Mstr.t;
    ns_xs  : xsymbol   Mstr.t;
    ns_ns  : namespace Mstr.t;
    ns_tns : namespace Mstr.t
}

let empty_ns = {
    ns_ts  = Mstr.empty;
    ns_ls  = Mstr.empty;
    ns_xs  = Mstr.empty;
    ns_ns  = Mstr.empty;
    ns_tns = Mstr.empty
  }

exception TypeNameClash of string

let ns_add_ts ns s ts =
  if Mstr.mem s ns.ns_ts then
    if ts_equal (Mstr.find s ns.ns_ts) ts
    then ns else error ~loc:ts.ts_ident.id_loc (TypeNameClash s)
  else
    {ns with ns_ts = Mstr.add s ts ns.ns_ts}
let ns_add_ls ns s ls = {ns with ns_ls = Mstr.add s ls ns.ns_ls}
let ns_add_xs ns s xs = {ns with ns_xs = Mstr.add s xs ns.ns_xs}
let ns_add_ns ns s new_ns =
  {ns with ns_ns = Mstr.add s new_ns ns.ns_ns}
let ns_add_tns ns s tns =
  {ns with ns_tns = Mstr.add s tns ns.ns_tns}

let merge_ns from_ns to_ns =
  let choose_fst _ x _ = Some x in
  let union m1 m2 = Mstr.union choose_fst m1 m2 in
  { ns_ts  = union from_ns.ns_ts to_ns.ns_ts;
    ns_ls  = union from_ns.ns_ls to_ns.ns_ls;
    ns_xs  = union from_ns.ns_xs to_ns.ns_xs;
    ns_ns  = union from_ns.ns_ns to_ns.ns_ns;
    ns_tns = union from_ns.ns_tns to_ns.ns_tns;}

let rec ns_find get_map ns = function
  | [] -> assert false
  | [x] -> Mstr.find x (get_map ns)
  | x::xs -> ns_find get_map (Mstr.find x ns.ns_ns) xs

let ns_find_ts ns s = ns_find (fun ns -> ns.ns_ts)   ns s
let ns_find_ls ns s = ns_find (fun ns -> ns.ns_ls)   ns s
let ns_find_xs ns s = ns_find (fun ns -> ns.ns_xs)   ns s
let ns_find_ns ns s = ns_find (fun ns -> ns.ns_ns)   ns s
let ns_find_tns ns s = ns_find (fun ns -> ns.ns_tns) ns s

let rec ns_rm_ts ns = function
  | [] -> assert false
  | [x] -> {ns with ns_ts = Mstr.remove x ns.ns_ts}
  | x :: xs ->
     let x_ns = ns_rm_ts (Mstr.find x ns.ns_ns) xs in
     {ns with ns_ns = Mstr.add x x_ns ns.ns_ns}

let rec ns_replace_ts new_ts sl ns = match sl with
  | [] -> assert false
  | [x] -> {ns with ns_ts = Mstr.add x new_ts ns.ns_ts}
  | x :: xs ->
     let ns_ns = Mstr.find x ns.ns_ns in
     let ns_ns = ns_replace_ts new_ts xs ns_ns in
     { ns with ns_ns = Mstr.add x ns_ns ns.ns_ns }

let rec ns_subst_ts old_ns new_ts {ns_ts;ns_ls;ns_xs;ns_ns;ns_tns} =
  {ns_ts = Mstr.map (ts_subst_ts old_ns new_ts) ns_ts;
   ns_ls = Mstr.map (ls_subst_ts old_ns new_ts) ns_ls;
   ns_xs = Mstr.map (xs_subst_ts old_ns new_ts) ns_xs;
   ns_ns = Mstr.map (ns_subst_ts old_ns new_ts) ns_ns;
   ns_tns = Mstr.map (ns_subst_ts old_ns new_ts) ns_tns}

let rec ns_subst_ty old_ts new_ts ty {ns_ts;ns_ls;ns_xs;ns_ns;ns_tns} =
  {ns_ts = Mstr.map (ts_subst_ty old_ts new_ts ty) ns_ts;
   ns_ls = Mstr.map (ls_subst_ty old_ts new_ts ty) ns_ls;
   ns_xs = Mstr.map (xs_subst_ty old_ts new_ts ty) ns_xs;
   ns_ns = Mstr.map (ns_subst_ty old_ts new_ts ty) ns_ns;
   ns_tns = Mstr.map (ns_subst_ty old_ts new_ts ty) ns_tns
  }

(* let rec ns_subst_ts sl ns ts = match sl with
 *   | [] -> assert false
 *   | [s] ->
 *      let old_ts = Mstr.find s ns.ns_ts in
 *      check_report ~loc:ts.ts_ident.id_loc
 *        (ts_arity old_ts = ts_arity ts) "type arity do not match";
 *      let ts = {old_ts with ts_args = ts.ts_args;
 *                            ts_alias = ts.ts_alias} in
 *      {ns with ns_ts = Mstr.add s ts ns.ns_ts }
 *   | x :: xs ->
 *      let x_ns = ns_subst_ts xs (Mstr.find x ns.ns_ns) ts in
 *      {ns with ns_ns = Mstr.add x x_ns ns.ns_ns} *)

(** Modules *)

type known_ids = signature_item Mid.t

type module_uc = {
    md_nm      : ident;
    md_sigs    : signature list;
    md_prefix  : string    list;
    md_import  : namespace list;
    md_export  : namespace list;
    md_kid     : known_ids;
    md_crcm    : Coercion.t
}

let module_uc md_nm md_sigs md_prefix md_import md_export md_kid md_crcm =
  {md_nm;md_sigs;md_prefix;md_import;md_export;md_kid;md_crcm}

let md_add ns_add md s x =
  match md.md_import, md.md_export with
  | i0 :: il, e0 :: el ->
     {md with md_import  = ns_add i0 s x :: il;
              md_export = ns_add e0 s x :: el}
  | _ -> assert false

let add_ts   = md_add ns_add_ts
let add_ls   = md_add ns_add_ls
let add_xs   = md_add ns_add_xs
let add_ns   = md_add ns_add_ns
let add_tns  = md_add ns_add_tns

let add_kid md id s =
  {md with md_kid = Mid.add id s md.md_kid}

let add_sig md sig_ =
  match md.md_sigs with
  | s0 :: sl ->
     {md with md_sigs = (sig_ :: s0) :: sl}
  | _ -> assert false

let add_coer md ls =
  {md with md_crcm = Coercion.add md.md_crcm ls}

let add_ns_top md ns =
  let add f md map = Mstr.fold (fun s v md -> f md s v) map md in
  let md = add add_ts  md ns.ns_ts in
  let md = add add_ls  md ns.ns_ls in
  let md = add add_xs  md ns.ns_xs in
  let md = add add_ns  md ns.ns_ns in
  let md = add add_tns md ns.ns_tns in
  md

let md_replace_ts md new_ts sl =
  match md.md_import, md.md_export with
  | i0 :: il, e0 :: el ->
     {md with md_import  = ns_replace_ts new_ts sl i0 :: il;
              md_export = ns_replace_ts new_ts sl e0 :: el}
  | _ -> assert false

let md_subst_ts md old_ts new_ts =
  match md.md_import, md.md_export with
  | i0 :: il, e0 :: el ->
     {md with
       md_import  = ns_subst_ts old_ts new_ts i0 :: il;
       md_export = ns_subst_ts old_ts new_ts e0 :: el}
  | _ -> assert false

let md_subst_ty md old_ts new_ts ty =
  match md.md_import, md.md_export with
  | i0 :: il, e0 :: el ->
     {md with md_import  = ns_subst_ty old_ts new_ts ty i0 :: il;
              md_export = ns_subst_ty old_ts new_ts ty e0 :: el}
  | _ -> assert false

let md_rm_ts md sl =
  match md.md_import, md.md_export with
  | i0 :: il, e0 :: el ->
     {md with md_import  = ns_rm_ts i0 sl :: il;
              md_export = ns_rm_ts e0 sl :: el}
  | _ -> assert false

let open_module md s =
  match md.md_import with
  | i0 :: _ ->
     {md with md_prefix = s :: md.md_prefix;
              md_sigs   = [] :: md.md_sigs;
              md_import  = i0 :: md.md_import;
              md_export = empty_ns :: md.md_export}
  | _ -> assert false

let close_module md =
  match md.md_import, md.md_export, md.md_prefix, md.md_sigs with
  | _ :: i1 :: il, e0 :: e1 :: el, p0 :: pl, _ :: sl ->
     {md with md_prefix = pl;
              md_import  = ns_add_ns i1 p0 e0 :: il;
              md_export = ns_add_ns e1 p0 e0 :: el;
              md_sigs   = sl;}
  | _ -> assert false

let close_module_functor md =
  match md.md_import, md.md_export, md.md_prefix, md.md_sigs with
  | _ :: i1 :: il, e0 :: e1 :: el, p0 :: pl, _ :: sl ->
     {md with md_prefix = pl;
              md_import  = ns_add_ns i1 p0 e0 :: il;
              md_export = e1 :: el;
              md_sigs   = sl;}
  | _ -> assert false

let close_module_use md =
  match md.md_import, md.md_export, md.md_prefix, md.md_sigs with
  | _ :: i1 :: il, e0 :: e1 :: el, p0 :: pl, _ :: sl ->
     let i1, e1 = merge_ns e0 i1, merge_ns e0 e1 in (* ERROR we should not merge the e0 and 01 *)
     let i1, e1 = ns_add_ns i1 p0 e0, ns_add_ns e1 p0 e0 in  (* ERROR we should not merge the e0 and 01 *)
     {md with md_prefix = pl;
              md_import  = i1 :: il;
              md_export = e1 :: el;
              md_sigs   = sl}
  | _ -> assert false

let close_module_type md =
  match md.md_import, md.md_export, md.md_prefix, md.md_sigs with
  | _ :: i1 :: il, e0 :: e1 :: el, p0 :: pl, _ :: sl ->
     {md with md_prefix = pl;
              md_import  = ns_add_tns i1 p0 e0 :: il;
              md_export = ns_add_tns e1 p0 e0 :: el;
              md_sigs   = sl;}
  | _ -> assert false

let wrap_up_module md = match md.md_sigs with
  | [s] -> {md with md_sigs = [List.rev s]}
  | _ -> assert false

let get_top_sigs md = match md.md_sigs with
  | s0 :: _ -> List.rev s0
  | _ -> assert false

let get_top_import md = match md.md_import with
  | i0 :: _ -> i0
  | _ -> assert false

(* let add_ns_to_md ns md =
 *   let md_import = joimport md.md_import ns in
 *   { md with md_import } *)

(* let add_md f kid ns md =
 *   let md = add_ns_to_md ns md in
 *   let md = {md with md_import = ns_add_ns md.md_import f ns} in
 *   let combine id sig1 sig2 = assert (sig1 = sig2); Some sig1 in
 *   (\* CHECK we taking all the known ids from source, but in fact we
 *      just need those from ml *\)
 *   let kid = Mid.union combine kid md.md_kid in
 *   {md with md_kid = kid} *)

let add_sig_contents md sig_ =
  let md = add_sig md sig_ in
  let get_cs_pjs = function
    | Pty_abstract -> []
    | Pty_variant cdl ->
       List.map (fun cd -> cd.cd_cs) cdl
    | Pty_record rd ->
       rd.rd_cs :: (List.map (fun ld -> ld.ld_field) rd.rd_ldl)
    | _ -> assert false in
  match sig_.sig_desc with
  | Sig_function f ->
     let md = add_ls md f.fun_ls.ls_name.id_str f.fun_ls in
     let md =
       if f.fun_spec.fun_coer then add_coer md f.fun_ls else md in
     add_kid md f.fun_ls.ls_name sig_
  | Sig_type (rf,tdl,g) ->
     let add_td md td =
       let s = (ts_ident td.td_ts).id_str in
       let md = add_ts md s td.td_ts in
       let csl = get_cs_pjs td.td_kind in
       let md = List.fold_left (fun md cs ->
         add_ls md cs.ls_name.id_str cs) md csl in
       let md = List.fold_left (fun md ls ->
         add_ls md ls.ls_name.id_str ls) md td.td_spec.ty_field in
       add_kid md td.td_ts.ts_ident sig_  in
     List.fold_left add_td md tdl
  | Sig_exception te ->
     let s = te.exn_constructor.ext_ident.id_str in
     let xs = te.exn_constructor.ext_xs in
     let md = add_xs md s xs in
     add_kid md te.exn_constructor.ext_ident sig_
  | _ -> md (* TODO *)

(** Primitives types and functions *)

let ns_with_primitives =
  (* reason for following types to be built-in:
      integer, string, float, char, unit - constants;
      bool   - being able to automatize the conversion between
      formulas and bool;
      option - allow for optional parameters;
      list   - the constructors ([], and ::) cannot be declared in
      the library with the flag ~constr:true
   *)
  let primitive_tys =
    [ ("integer", ts_integer); ("string", ts_string); ("char", ts_char);
      ("float", ts_float); ("bool", ts_bool); ("option",ts_option);
      ("list", ts_list); ("unit", ts_unit)] in
  let primitive_ps =
    [ ps_equ.ls_name.id_str, ps_equ;
      (lt.id_str, psymbol lt     [ty_integer;ty_integer]);
      (le.id_str, psymbol le     [ty_integer;ty_integer]);
      (gt.id_str, psymbol gt     [ty_integer;ty_integer]);
      (ge.id_str, psymbol ge     [ty_integer;ty_integer])
    ] in
  let primitive_ls =
    [ (plus.id_str, fsymbol plus   [ty_integer;ty_integer] ty_integer);
      (minus.id_str, fsymbol minus [ty_integer;ty_integer] ty_integer);
      (mult.id_str, fsymbol mult   [ty_integer;ty_integer] ty_integer);
      (div.id_str, fsymbol div   [ty_integer;ty_integer] ty_integer);
      (umin.id_str, fsymbol umin   [ty_integer] ty_integer);
      (let tv = fresh_ty_var "a" in
       none.id_str, fsymbol ~constr:true none [] (ty_option tv));
      (let tv = fresh_ty_var "a" in
       some.id_str, fsymbol ~constr:true some [tv] (ty_option tv));
      (let tv = fresh_ty_var "a" in
       nil.id_str, fsymbol ~constr:true nil [] (ty_list tv));
      (let tv = fresh_ty_var "a" in
       cons.id_str, fsymbol ~constr:true cons [tv; ty_app ts_list [tv]] (ty_list tv));
    ] in
  let ns = List.fold_left (fun ns (s,ts) ->
               ns_add_ts ns s ts) empty_ns primitive_tys in
  List.fold_left (fun ns (s,ls) ->
      ns_add_ls ns s ls) ns (primitive_ls @ primitive_ps)

let md_with_primitives s =
  module_uc (fresh_id s) [[]] [s] [ns_with_primitives] [empty_ns]
    Mid.empty Coercion.empty

(** Pretty printing *)

open Opprintast

let rec tree_ns f fmt ns =
  Mstr.iter (fun s ns ->
      if f ns = Mstr.empty then
        pp fmt "@[%s@\n@]" s
      else
        pp fmt "@[%s:@\n@ @[%a@]@\n@]" s (tree_ns f) (f ns)) ns

let rec ns_names nsm =
  List.map fst (Mstr.bindings nsm)

let print_mstr_vals printer fmt m =
  let print_elem e = pp fmt "@[%a@]@\n" printer e in
  Mstr.iter (fun _ -> print_elem) m

let rec print_nested_ns fmt ns =
  let print_elem nm e = pp fmt "@[%a@]@\n" (print_ns nm) e in
  Mstr.iter (fun nm ns -> print_elem nm ns) ns

and print_ns nm fmt {ns_ts;ns_ls;ns_xs;ns_ns;ns_tns} =
  pp fmt "@[@[<hv2>@[Namespace: %s@]@\n\
          @[<hv2>Type symbols@\n%a@]@\n\
          @[<hv2>Logic Symbols@\n%a@]@\n\
          @[<hv2>Exception Symbols@\n%a@]@\n\
          @[<hv2>Namespaces@\n%a@]@\n\
          @[<hv2>Type Namespaces@\n%a@]\
          @]\
          @]"
    nm
    (print_mstr_vals print_ts) ns_ts
    (print_mstr_vals print_ls_decl) ns_ls
    (print_mstr_vals print_xs) ns_xs
    print_nested_ns ns_ns
    print_nested_ns ns_tns
    (* (tree_ns (fun ns -> ns.ns_ns)) ns_ns
     * (tree_ns (fun ns -> ns.ns_tns)) ns_tns *)

let rec print_mod fmt {md_nm;md_sigs;md_export;md_crcm} =
  match md_export, md_sigs with
  | e0 :: _, s0 :: _ ->
     pp fmt "@[module %a@\n@[<h2>@\n%a@\n@[<hv2>Coercions@\n%a@]@\n@[<hv2>Signatures@\n%a@]@]@]@."
       print_ident md_nm
       (print_ns md_nm.id_str) e0
       Coercion.print_coercions md_crcm
       print_signature s0
  | _ -> assert false

let () =
  let open Location in
  register_error_of_exn (function
      | TypeNameClash s ->
         Some (errorf "Multiple definitions of type %s" s)
      | _ -> None)
