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
open Identifier
open Ttypes
open Symbols
open Tterm_printer
open Tast
open Tast_helper

(** Namespace *)

module Mstr = Map.Make (String)

type namespace = {
  ns_ts : tysymbol Mstr.t;
  ns_ls : lsymbol Mstr.t;
  ns_fd : lsymbol Mstr.t;
  ns_xs : xsymbol Mstr.t;
  ns_ns : namespace Mstr.t;
  ns_tns : namespace Mstr.t;
}

let empty_ns =
  {
    ns_ts = Mstr.empty;
    ns_ls = Mstr.empty;
    ns_fd = Mstr.empty;
    ns_xs = Mstr.empty;
    ns_ns = Mstr.empty;
    ns_tns = Mstr.empty;
  }

let add ~allow_duplicate ~equal ~loc ns s x =
  if allow_duplicate then Mstr.add s x ns
  else
    match Mstr.find s ns with
    | t when not (equal t x) -> W.error ~loc (W.Name_clash s)
    | _ | (exception Not_found) -> Mstr.add s x ns

let ns_add_ts ~allow_duplicate ns s ts =
  let ns_ts =
    add ~allow_duplicate ~equal:ts_equal ~loc:ts.ts_ident.id_loc ns.ns_ts s ts
  in
  { ns with ns_ts }

let ns_add_ls ~allow_duplicate:_ ns s ls =
  let ns_ls =
    add ~allow_duplicate:true ~equal:ls_equal ~loc:ls.ls_name.id_loc ns.ns_ls s
      ls
  in
  { ns with ns_ls }

let ns_add_fd ~allow_duplicate:_ ns s fd =
  let ns_fd =
    add ~allow_duplicate:true ~equal:ls_equal ~loc:fd.ls_name.id_loc ns.ns_fd s
      fd
  in
  { ns with ns_fd }

let ns_add_xs ~allow_duplicate ns s xs =
  let ns_xs =
    add ~allow_duplicate ~equal:xs_equal ~loc:xs.xs_ident.id_loc ns.ns_xs s xs
  in
  { ns with ns_xs }

(* FIXME: Adding multiple modules with the same name to the export should not be
   allowed either. *)
let ns_add_ns ~allow_duplicate:_ ns s new_ns =
  { ns with ns_ns = Mstr.add s new_ns ns.ns_ns }

let ns_add_tns ~allow_duplicate:_ ns s tns =
  { ns with ns_tns = Mstr.add s tns ns.ns_tns }

let merge_ns from_ns to_ns =
  let choose_fst _ x _ = Some x in
  let union m1 m2 = Mstr.union choose_fst m1 m2 in
  {
    ns_ts = union from_ns.ns_ts to_ns.ns_ts;
    ns_ls = union from_ns.ns_ls to_ns.ns_ls;
    ns_fd = union from_ns.ns_fd to_ns.ns_fd;
    ns_xs = union from_ns.ns_xs to_ns.ns_xs;
    ns_ns = union from_ns.ns_ns to_ns.ns_ns;
    ns_tns = union from_ns.ns_tns to_ns.ns_tns;
  }

let rec ns_find get_map ns = function
  | [] -> assert false
  | [ x ] -> Mstr.find x (get_map ns)
  | x :: xs -> ns_find get_map (Mstr.find x ns.ns_ns) xs

let ns_find_ts ns s = ns_find (fun ns -> ns.ns_ts) ns s
let ns_find_ls ns s = ns_find (fun ns -> ns.ns_ls) ns s
let ns_find_fd ns s = ns_find (fun ns -> ns.ns_fd) ns s
let ns_find_xs ns s = ns_find (fun ns -> ns.ns_xs) ns s
let ns_find_ns ns s = ns_find (fun ns -> ns.ns_ns) ns s
let ns_find_tns ns s = ns_find (fun ns -> ns.ns_tns) ns s
let ns_exists_ns ns s = Mstr.mem s ns.ns_ns

let rec ns_rm_ts ns = function
  | [] -> assert false
  | [ x ] -> { ns with ns_ts = Mstr.remove x ns.ns_ts }
  | x :: xs ->
      let x_ns = ns_rm_ts (Mstr.find x ns.ns_ns) xs in
      { ns with ns_ns = Mstr.add x x_ns ns.ns_ns }

let rec ns_replace_ts new_ts sl ns =
  match sl with
  | [] -> assert false
  | [ x ] -> { ns with ns_ts = Mstr.add x new_ts ns.ns_ts }
  | x :: xs ->
      let ns_ns = Mstr.find x ns.ns_ns in
      let ns_ns = ns_replace_ts new_ts xs ns_ns in
      { ns with ns_ns = Mstr.add x ns_ns ns.ns_ns }

let rec ns_subst_ts old_ns new_ts { ns_ts; ns_ls; ns_fd; ns_xs; ns_ns; ns_tns }
    =
  {
    ns_ts = Mstr.map (ts_subst_ts old_ns new_ts) ns_ts;
    ns_ls = Mstr.map (ls_subst_ts old_ns new_ts) ns_ls;
    ns_fd = Mstr.map (ls_subst_ts old_ns new_ts) ns_fd;
    ns_xs = Mstr.map (xs_subst_ts old_ns new_ts) ns_xs;
    ns_ns = Mstr.map (ns_subst_ts old_ns new_ts) ns_ns;
    ns_tns = Mstr.map (ns_subst_ts old_ns new_ts) ns_tns;
  }

let rec ns_subst_ty old_ts new_ts ty
    { ns_ts; ns_ls; ns_fd; ns_xs; ns_ns; ns_tns } =
  {
    ns_ts = Mstr.map (ts_subst_ty old_ts new_ts ty) ns_ts;
    ns_ls = Mstr.map (ls_subst_ty old_ts new_ts ty) ns_ls;
    ns_fd = Mstr.map (ls_subst_ty old_ts new_ts ty) ns_fd;
    ns_xs = Mstr.map (xs_subst_ty old_ts new_ts ty) ns_xs;
    ns_ns = Mstr.map (ns_subst_ty old_ts new_ts ty) ns_ns;
    ns_tns = Mstr.map (ns_subst_ty old_ts new_ts ty) ns_tns;
  }

(** Primitives types and functions *)

let ns_with_primitives =
  (* reason for the following types to be built-in:
      integer, string, float, char, unit - constants;
      bool   - being able to automatize the conversion between
      formulas and bool;
      option - allow for optional parameters;
      list - we cannot declare it in the list library for the following
      reasons:
        (1) if we try to declare "type 'a list = [] | (::) of 'a * 'a
        list" in the "list.mli" library file, the declaration will be
        parsed by the OCaml parser, which names the second constructor as
        "::". The GOSPEL parser names it "infix ::";
        (2) as alternative, we could leave the type abstract and create
        function ([]) and (::). However, in this case these functions
        could not be used in pattern matching because they are not
        constructors. *)
  let primitive_tys =
    [
      ("integer", ts_integer);
      ("int", ts_int);
      ("string", ts_string);
      ("char", ts_char);
      ("float", ts_float);
      ("bool", ts_bool);
      ("option", ts_option);
      ("list", ts_list);
      ("unit", ts_unit);
    ]
  in
  let primitive_ps = [ (ps_equ.ls_name.id_str, ps_equ) ] in
  let primitive_ls =
    let tv_option =
      match ts_option.ts_args with [ v ] -> ty_of_var v | _ -> assert false
    in
    let tv_list =
      match ts_list.ts_args with [ v ] -> ty_of_var v | _ -> assert false
    in
    [
      ( none.id_str,
        fsymbol ~constr:true ~field:false none [] (ty_option tv_option) );
      ( some.id_str,
        fsymbol ~constr:true ~field:false some [ tv_option ]
          (ty_option tv_option) );
      (nil.id_str, fsymbol ~constr:true ~field:false nil [] (ty_list tv_list));
      ( cons.id_str,
        fsymbol ~constr:true ~field:false cons
          [ tv_list; ty_list tv_list ]
          (ty_list tv_list) );
    ]
  in
  let ns =
    List.fold_left
      (fun ns (s, ts) -> ns_add_ts ~allow_duplicate:true ns s ts)
      empty_ns primitive_tys
  in
  List.fold_left
    (fun ns (s, ls) -> ns_add_ls ~allow_duplicate:true ns s ls)
    ns
    (primitive_ls @ primitive_ps)

(** Modules *)

module Mid = Map.Make (Ident)

type known_ids = signature_item Mid.t
type file = { fl_nm : Ident.t; fl_sigs : signature; fl_export : namespace }

type module_uc = {
  muc_nm : Ident.t;
  muc_sigs : signature list;
  muc_prefix : string list;
  (* essential when closing namespaces *)
  muc_import : namespace list;
  muc_export : namespace list;
  muc_files : file Mstr.t;
  muc_kid : known_ids;
  muc_crcm : Coercion.t;
}

let muc_add ?(export = false) add muc s x =
  match (muc.muc_import, muc.muc_export) with
  | i0 :: il, e0 :: el ->
      let i = add ~allow_duplicate:true i0 s x in
      let e = if export then add ~allow_duplicate:false e0 s x else e0 in
      { muc with muc_import = i :: il; muc_export = e :: el }
  | _ -> assert false

let add_ts ?(export = false) = muc_add ~export ns_add_ts
let add_ls ?(export = false) = muc_add ~export ns_add_ls
let add_fd ?(export = false) = muc_add ~export ns_add_fd
let add_xs ?(export = false) = muc_add ~export ns_add_xs
let add_ns ?(export = false) = muc_add ~export ns_add_ns
let add_tns ?(export = false) = muc_add ~export ns_add_tns
let add_file muc s file = { muc with muc_files = Mstr.add s file muc.muc_files }
let get_file muc s = Mstr.find s muc.muc_files
let add_kid muc id s = { muc with muc_kid = Mid.add id s muc.muc_kid }

let add_sig muc sig_ =
  match muc.muc_sigs with
  | s0 :: sl -> { muc with muc_sigs = (sig_ :: s0) :: sl }
  | _ -> assert false

let add_coer muc ls = { muc with muc_crcm = Coercion.add muc.muc_crcm ls }

let add_ns_top ?(export = false) muc ns =
  let add f muc map = Mstr.fold (fun s v muc -> f muc s v) map muc in
  let muc = add (add_ts ~export) muc ns.ns_ts in
  let muc = add (add_ls ~export) muc ns.ns_ls in
  let muc = add (add_fd ~export) muc ns.ns_fd in
  let muc = add (add_xs ~export) muc ns.ns_xs in
  let muc = add (add_ns ~export) muc ns.ns_ns in
  let muc = add (add_tns ~export) muc ns.ns_tns in
  muc

let muc_replace_ts muc new_ts sl =
  match (muc.muc_import, muc.muc_export) with
  | i0 :: il, e0 :: el ->
      {
        muc with
        muc_import = ns_replace_ts new_ts sl i0 :: il;
        muc_export = ns_replace_ts new_ts sl e0 :: el;
      }
  | _ -> assert false

let muc_subst_ts muc old_ts new_ts =
  match (muc.muc_import, muc.muc_export) with
  | i0 :: il, e0 :: el ->
      {
        muc with
        muc_import = ns_subst_ts old_ts new_ts i0 :: il;
        muc_export = ns_subst_ts old_ts new_ts e0 :: el;
      }
  | _ -> assert false

let muc_subst_ty muc old_ts new_ts ty =
  match (muc.muc_import, muc.muc_export) with
  | i0 :: il, e0 :: el ->
      {
        muc with
        muc_import = ns_subst_ty old_ts new_ts ty i0 :: il;
        muc_export = ns_subst_ty old_ts new_ts ty e0 :: el;
      }
  | _ -> assert false

let muc_rm_ts muc sl =
  match (muc.muc_import, muc.muc_export) with
  | i0 :: il, e0 :: el ->
      {
        muc with
        muc_import = ns_rm_ts i0 sl :: il;
        muc_export = ns_rm_ts e0 sl :: el;
      }
  | _ -> assert false

let open_empty_module muc s =
  {
    muc with
    muc_prefix = s :: muc.muc_prefix;
    muc_sigs = [] :: muc.muc_sigs;
    muc_import = ns_with_primitives :: muc.muc_import;
    muc_export = empty_ns :: muc.muc_export;
  }

let close_module_file muc =
  match (muc.muc_import, muc.muc_export, muc.muc_prefix, muc.muc_sigs) with
  | _ :: i1 :: il, e0 :: e1 :: el, p0 :: pl, s0 :: sl ->
      let file =
        {
          fl_nm = Ident.create ~loc:Location.none p0;
          fl_sigs = List.rev s0;
          fl_export = e0;
        }
      in
      {
        muc with
        muc_prefix = pl;
        muc_import = ns_add_ns ~allow_duplicate:true i1 p0 e0 :: il;
        muc_export = e1 :: el;
        muc_sigs = sl;
        muc_files = Mstr.add p0 file muc.muc_files;
      }
  | _ -> assert false

let open_module muc s =
  match muc.muc_import with
  | i0 :: _ ->
      {
        muc with
        muc_prefix = s :: muc.muc_prefix;
        muc_sigs = [] :: muc.muc_sigs;
        muc_import = i0 :: muc.muc_import;
        muc_export = empty_ns :: muc.muc_export;
      }
  | _ -> assert false

(* for module declarations *)
let close_module muc =
  match (muc.muc_import, muc.muc_export, muc.muc_prefix, muc.muc_sigs) with
  | _ :: i1 :: il, e0 :: e1 :: el, p0 :: pl, _ :: sl ->
      {
        muc with
        muc_prefix = pl;
        muc_import = ns_add_ns ~allow_duplicate:true i1 p0 e0 :: il;
        muc_export = ns_add_ns ~allow_duplicate:true e1 p0 e0 :: el;
        muc_sigs = sl;
      }
  | _ -> assert false

(* for functor arguments *)
let close_module_functor muc =
  match (muc.muc_import, muc.muc_export, muc.muc_prefix, muc.muc_sigs) with
  | _ :: i1 :: il, e0 :: e1 :: el, p0 :: pl, _ :: sl ->
      {
        muc with
        muc_prefix = pl;
        muc_import = ns_add_ns ~allow_duplicate:true i1 p0 e0 :: il;
        muc_export = e1 :: el;
        muc_sigs = sl;
      }
  | _ -> assert false

(* for module types *)
let close_module_type muc =
  match (muc.muc_import, muc.muc_export, muc.muc_prefix, muc.muc_sigs) with
  | _ :: i1 :: il, e0 :: e1 :: el, p0 :: pl, _ :: sl ->
      {
        muc with
        muc_prefix = pl;
        muc_import = ns_add_tns ~allow_duplicate:true i1 p0 e0 :: il;
        muc_export = ns_add_tns ~allow_duplicate:true e1 p0 e0 :: el;
        muc_sigs = sl;
      }
  | _ -> assert false

let get_top_sigs muc =
  match muc.muc_sigs with s0 :: _ -> List.rev s0 | _ -> assert false

let get_top_import muc =
  match muc.muc_import with i0 :: _ -> i0 | _ -> assert false

let get_top_export muc =
  match muc.muc_export with e0 :: _ -> e0 | _ -> assert false

let add_value muc sig_ v =
  let spec = Attrs.typed_of_val_spec v in
  match spec with
  | Some sp when sp.sp_pure ->
      let tyl = List.map ty_of_lb_arg sp.sp_args in
      let ty = ty_tuple (List.map ty_of_lb_arg sp.sp_ret) in
      let ls = lsymbol ~field:false v.vd_name tyl (Some ty) in
      let muc = add_ls ~export:true muc ls.ls_name.id_str ls in
      add_kid muc ls.ls_name sig_
  | _ -> muc

let add_type muc sig_ tdl =
  let get_cs_pjs = function
    | Ptype_abstract | Ptype_open -> []
    | Ptype_variant cdl -> List.map (fun cd -> cd.cd_cs) cdl
    | Ptype_record rd -> rd.rd_cs :: List.map (fun ld -> ld.ld_field) rd.rd_ldl
  in
  let add_td muc td =
    let s = (ts_ident td.td_ts).id_str in
    let muc = add_ts ~export:true muc s td.td_ts in
    let csl = get_cs_pjs td.td_kind in
    let muc =
      List.fold_left
        (fun muc cs ->
          (if cs.ls_field then add_fd else add_ls)
            ~export:true muc cs.ls_name.id_str cs)
        muc csl
    in
    let fields =
      Option.fold ~none:[]
        ~some:(fun spec -> List.map fst spec.ty_fields)
        td.td_spec
    in
    let muc =
      List.fold_left
        (fun muc ls ->
          (if ls.ls_field then add_fd else add_ls)
            ~export:true muc ls.ls_name.id_str ls)
        muc fields
    in
    add_kid muc td.td_ts.ts_ident sig_
  in
  List.fold_left add_td muc tdl

let add_function muc sig_ f =
  let muc = add_ls ~export:true muc f.fun_ls.ls_name.id_str f.fun_ls in
  let muc =
    match f.fun_spec with
    | Some spec when spec.fun_coer -> add_coer muc f.fun_ls
    | _ -> muc
  in
  add_kid muc f.fun_ls.ls_name sig_

let add_open muc sig_ (o : open_description) =
  let nm = List.hd (List.rev opn_id) in
  let ns = ns_find_ns (get_top_import muc) opn_id in
  add_ns_top ~export:false (add_ns ~export:false muc nm ns) ns

let add_sig_contents muc sig_ =
  let muc = add_sig muc sig_ in
  match sig_.psig_desc with
  | Psig_value v -> add_value muc sig_ v
  | Psig_attribute a when Attrs.is_typed_spec a -> (
      match Attrs.of_typed_floating sig_.psig_desc with
      | Function f -> add_function muc sig_ f
      | Axiom _ -> muc
      | Type (_, tl) -> add_type muc sig_ tl
      | Value v -> add_value muc sig_ v
      | Open o -> add_open muc sig_ o)
  | Psig_type (_, tdl) -> add_type muc sig_ tdl
  | Psig_exception te ->
      let s = te.ptyexn_constructor.pext_name.txt in
      let xs = te.exn_constructor.ext_xs in
      let muc = add_xs ~export:true muc s xs in
      add_kid muc te.exn_constructor.ext_ident sig_
  | Psig_open o -> add_open muc sig_ o
  | _ -> muc

(** Module under construction with primitive types and functions *)

let init_muc s =
  {
    muc_nm = Ident.create ~loc:Location.none s;
    muc_sigs = [ [] ];
    muc_prefix = [ s ];
    muc_import = [ ns_with_primitives ];
    muc_export = [ empty_ns ];
    muc_files = Mstr.empty;
    muc_kid = Mid.empty;
    muc_crcm = Coercion.empty;
  }

let wrap_up_muc (muc : module_uc) =
  match (muc.muc_export, muc.muc_sigs) with
  | [ e ], [ s ] -> { fl_nm = muc.muc_nm; fl_sigs = List.rev s; fl_export = e }
  | _ -> assert false

(** Pretty printing *)

open Utils.Fmt
open Tast_printer

let rec tree_ns f fmt ns =
  Mstr.iter
    (fun s ns ->
      if f ns = Mstr.empty then pp fmt "@[%s@\n@]" s
      else pp fmt "@[%s:@\n@ @[%a@]@\n@]" s (tree_ns f) (f ns))
    ns

let ns_names nsm = List.map fst (Mstr.bindings nsm)

let print_mstr_vals printer fmt m =
  let print_elem e = pp fmt "@[%a@]@\n" printer e in
  Mstr.iter (fun _ -> print_elem) m

let rec print_nested_ns fmt ns =
  let print_elem nm e = pp fmt "@[%a@]@\n" (print_ns nm) e in
  Mstr.iter (fun nm ns -> print_elem nm ns) ns

and print_ns nm fmt { ns_ts; ns_ls; ns_fd; ns_xs; ns_ns; ns_tns } =
  pp fmt
    "@[@[<hv2>@[Namespace: %s@]@\n\
     @[<hv2>Type symbols@\n\
     %a@]@\n\
     @[<hv2>Logic Symbols@\n\
     %a@]@\n\
     @[<hv2>Field Symbols@\n\
     %a@]@\n\
     @[<hv2>Exception Symbols@\n\
     %a@]@\n\
     @[<hv2>Namespaces@\n\
     %a@]@\n\
     @[<hv2>Type Namespaces@\n\
     %a@]@]@]"
    nm (print_mstr_vals print_ts) ns_ts
    (print_mstr_vals print_ls_decl)
    ns_ls
    (print_mstr_vals print_ls_decl)
    ns_fd (print_mstr_vals print_xs) ns_xs print_nested_ns ns_ns print_nested_ns
    ns_tns
(* (tree_ns (fun ns -> ns.ns_ns)) ns_ns
 * (tree_ns (fun ns -> ns.ns_tns)) ns_tns *)

let print_file fmt { fl_nm; fl_sigs; fl_export } =
  pp fmt "@[module %a@\n@[<h2>@\n%a@\n@[<hv2>Signatures@\n%a@]@]@]@." Ident.pp
    fl_nm (print_ns fl_nm.id_str) fl_export print_signature fl_sigs
