open Utils
open Identifier
open Ttypes
open Tterm
open Tast

(** Namespace *)

type namespace = {
    ns_ts : tysymbol  Mstr.t;
    ns_ls : lsymbol   Mstr.t;
    ns_xs : xsymbol   Mstr.t;
    ns_ns : namespace Mstr.t
}

let empty_ns = {
    ns_ts = Mstr.empty;
    ns_ls = Mstr.empty;
    ns_xs = Mstr.empty;
    ns_ns = Mstr.empty
  }

exception NameClash of string

let ns_add_ts ns s ts =
  if Mstr.mem s ns.ns_ts then raise (NameClash s) else
  {ns with ns_ts = Mstr.add s ts ns.ns_ts}
let ns_add_ls ns s ls = {ns with ns_ls = Mstr.add s ls ns.ns_ls}
let ns_add_xs ns s xs = {ns with ns_xs = Mstr.add s xs ns.ns_xs}
let ns_add_ns ns s new_ns =
  {ns with ns_ns = Mstr.add s new_ns ns.ns_ns}

let ns_with_primitives =
  (* There is a good reason for these types to be built-in: they are
     already declared in OCaml, and we want them to represent those
     same types. *)
  let primitive_tys =
    [ ("unit", ts_unit); ("integer", ts_integer); ("int", ts_int);
      ("string", ts_string); ("float", ts_float); ("bool", ts_bool);
      ("list", ts_list); ("option",ts_option)] in
  let primitive_ps =
    [ ps_equ.ls_name.id_str, ps_equ;
      (let id = fresh_id (infix "<") in
       id.id_str, psymbol id [ty_integer;ty_integer]);
      (let id = fresh_id (infix "<=") in
       id.id_str, psymbol id [ty_integer;ty_integer]);
      (let id = fresh_id (infix ">") in
       id.id_str, psymbol id [ty_integer;ty_integer]);
      (let id = fresh_id (infix ">=") in
       id.id_str, psymbol id [ty_integer;ty_integer]);
      (let id = fresh_id (infix "->") in
       id.id_str, psymbol id [ty_bool;ty_bool]);
    ] in
  let primitive_ls =
    [ (let id = fresh_id (infix "+") in
       id.id_str, fsymbol id [ty_integer;ty_integer] ty_integer);
      (let id = fresh_id (infix "-") in
       id.id_str, fsymbol id [ty_integer;ty_integer] ty_integer);
      (let id = fresh_id (infix "*") in
       id.id_str, fsymbol id [ty_integer;ty_integer] ty_integer);
      (let id = fresh_id "None" in
       let tv = fresh_ty_var "a" in
       id.id_str, fsymbol ~constr:true id [] (ty_option tv));
      (let id = fresh_id "Some" in
       let tv = fresh_ty_var "a" in
       id.id_str, fsymbol ~constr:true id [tv] (ty_option tv));
      (let id = fresh_id "[]" in
       let tv = fresh_ty_var "a" in
       id.id_str, fsymbol ~constr:true id [] (ty_list tv));
      (let id = fresh_id (infix "::") in
       let tv = fresh_ty_var "a" in
       id.id_str, fsymbol ~constr:true id [tv; ty_app ts_list [tv]] (ty_list tv));
    ] in
  let ns = List.fold_left (fun ns (s,ts) ->
               ns_add_ts ns s ts) empty_ns primitive_tys in
  List.fold_left (fun ns (s,ls) ->
      ns_add_ls ns s ls) ns (primitive_ls @ primitive_ps)

let rec ns_find get_map ns = function
  | [] -> assert false
  | [x] -> Mstr.find x (get_map ns)
  | x::xs -> ns_find get_map (Mstr.find x ns.ns_ns) xs

let ns_find_ts ns s = ns_find (fun ns -> ns.ns_ts) ns s
let ns_find_ls ns s = ns_find (fun ns -> ns.ns_ls) ns s
let ns_find_xs ns s = ns_find (fun ns -> ns.ns_xs) ns s
let ns_find_ns ns s = ns_find (fun ns -> ns.ns_ns) ns s

let join_ns old_ns new_ns =
  let choose_snd _ _ x = Some x in
  let union m1 m2 = Mstr.union choose_snd m1 m2 in
  { ns_ts = union old_ns.ns_ts new_ns.ns_ts;
    ns_ls = union old_ns.ns_ls new_ns.ns_ls;
    ns_xs = union old_ns.ns_xs new_ns.ns_xs;
    ns_ns = union old_ns.ns_ns new_ns.ns_ns;}

(** Modules *)

type known_ids = signature_item Mid.t

type module_ = {
    mod_nm   : ident;
    mod_sigs : signature;
    mod_ns   : namespace;
    mod_kid  : known_ids;
}

let empty_module id = {
    mod_nm   = id;
    mod_sigs = [];
    mod_ns   = empty_ns;
    mod_kid  = Mid.empty;
  }

let module_ mod_nm mod_sigs mod_ns mod_kid =
  {mod_nm;mod_sigs;mod_ns;mod_kid}

let md_with_primitives s =
  module_ (fresh_id s) [] ns_with_primitives Mid.empty

let md_find_ts md s = ns_find_ts md.mod_ns s
let md_find_ls md s = ns_find_ls md.mod_ns s
let md_find_xs md s = ns_find_xs md.mod_ns s
let md_find_ns md s = ns_find_ns md.mod_ns s

let md_find_id md id = Mid.find id md.mod_kid

let add_ts md s ts  = {md with mod_ns = ns_add_ts md.mod_ns s ts}
let add_ls md s ls  = {md with mod_ns = ns_add_ls md.mod_ns s ls}
let add_xs md s xs  = {md with mod_ns = ns_add_xs md.mod_ns s xs}
let add_kid md id s = {md with mod_kid = Mid.add id s md.mod_kid}
let add_sig md sig_ = {md with mod_sigs = sig_::md.mod_sigs}

let add_ns_to_md ns md =
  let mod_ns = join_ns md.mod_ns ns in
  { md with mod_ns }

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

let close_md md =
  {md with mod_sigs = List.rev md.mod_sigs}

(** Pretty printing *)

open Opprintast

let print_mstr_vals printer fmt m =
  let print_elem e = pp fmt "@[%a@]@\n" printer e in
  Mstr.iter (fun _ -> print_elem) m

let print_ns fmt {ns_ts;ns_ls;ns_xs;ns_ns} =
  assert (Mstr.is_empty ns_ns);
  pp fmt "@[Type symbols@\n%a@\nLogic Symbols@\n%a@\nException Symbols@\n%a@.@]"
    (print_mstr_vals print_ts) ns_ts
    (print_mstr_vals print_ls_decl) ns_ls
    (print_mstr_vals print_xs) ns_xs

let print_mod fmt {mod_nm;mod_sigs;mod_ns} =
  pp fmt "@[module %a\nNamespace@\n@[<h 2>@\n%a@]\nSignatures@\n@[<h 2>@\n%a@]@]"
    print_ident mod_nm
    print_ns mod_ns
    print_signature mod_sigs
