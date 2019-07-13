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

let merge_ns old_ns new_ns =
  let choose_snd _ _ x = Some x in
  let union m1 m2 = Mstr.union choose_snd m1 m2 in
  { ns_ts = union old_ns.ns_ts new_ns.ns_ts;
    ns_ls = union old_ns.ns_ls new_ns.ns_ls;
    ns_xs = union old_ns.ns_xs new_ns.ns_xs;
    ns_ns = union old_ns.ns_ns new_ns.ns_ns;}

let rec ns_find get_map ns = function
  | [] -> assert false
  | [x] -> Mstr.find x (get_map ns)
  | x::xs -> ns_find get_map (Mstr.find x ns.ns_ns) xs

let ns_find_ts ns s = ns_find (fun ns -> ns.ns_ts) ns s
let ns_find_ls ns s = ns_find (fun ns -> ns.ns_ls) ns s
let ns_find_xs ns s = ns_find (fun ns -> ns.ns_xs) ns s
let ns_find_ns ns s = ns_find (fun ns -> ns.ns_ns) ns s

(* let rec add_ns_under_name ml source target = match ml with
 *   | []    -> assert false
 *   | [s]   -> ns_add_ns target s source
 *   | x::xs ->
 *      let ns = try Mstr.find x target.ns_ns with
 *                 Not_found -> empty_ns in
 *      let ns = add_ns_under_name xs source ns in
 *      ns_add_ns target x ns *)

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
      (lt.id_str, psymbol lt     [ty_integer;ty_integer]);
      (le.id_str, psymbol le     [ty_integer;ty_integer]);
      (gt.id_str, psymbol gt     [ty_integer;ty_integer]);
      (ge.id_str, psymbol ge     [ty_integer;ty_integer]);
      (impl.id_str, psymbol impl [ty_bool;ty_bool]);
    ] in
  let primitive_ls =
    [ (plus.id_str, fsymbol plus   [ty_integer;ty_integer] ty_integer);
      (minus.id_str, fsymbol minus [ty_integer;ty_integer] ty_integer);
      (mult.id_str, fsymbol mult   [ty_integer;ty_integer] ty_integer);
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


(** Modules *)

type known_ids = signature_item Mid.t

(* and module_aaa =
 *   | Functor of module_aaa * module_aaa
 *   | Flat of module_typeee *)

type module_uc = {
    md_nm     : ident;
    md_sigs   : signature list;
    md_prefix : string    list;
    md_in_ns  : namespace list;
    md_out_ns : namespace list;
    md_kid    : known_ids;
}

let empty_module id = {
    md_nm     = id;
    md_sigs   = [];
    md_prefix = [id.id_str];
    md_in_ns  = [empty_ns];
    md_out_ns = [empty_ns];
    md_kid    = Mid.empty;
  }

let module_uc md_nm md_sigs md_prefix md_in_ns md_out_ns md_kid =
  {md_nm;md_sigs;md_prefix;md_in_ns;md_out_ns;md_kid}

let md_with_primitives s =
  module_uc (fresh_id s) [[]] [s] [ns_with_primitives] [empty_ns] Mid.empty

let md_add ns_add md s x =
  match md.md_in_ns, md.md_out_ns with
  | i0 :: ins, o0 :: ons ->
     {md with md_in_ns  = ns_add i0 s x :: ins;
              md_out_ns = ns_add o0 s x :: ons}
  | _ -> assert false

let add_ts  = md_add ns_add_ts
let add_ls  = md_add ns_add_ls
let add_xs  = md_add ns_add_xs
let add_ns  = md_add ns_add_ns
let add_kid md id s = {md with md_kid = Mid.add id s md.md_kid}
let add_sig md sig_ = match md.md_sigs with
  | s0 :: sl ->
     {md with md_sigs = (sig_ :: s0) :: sl}
  | _ -> assert false

let open_module md s =
  match md.md_in_ns with
  | i0 :: _ ->
     {md with md_prefix = s :: md.md_prefix;
              md_sigs   = [] :: md.md_sigs;
              md_in_ns  = i0 :: md.md_in_ns;
              md_out_ns = empty_ns :: md.md_out_ns}
  | _ -> assert false

let close_module md =
  match md.md_in_ns, md.md_out_ns, md.md_prefix, md.md_sigs with
  | _ :: i1 :: ins, o0 :: o1 :: ons, p0 :: pl, _ :: sl ->
     {md with md_prefix = pl;
              md_in_ns  = ns_add_ns i1 p0 o0 :: ins;
              md_out_ns = ns_add_ns o1 p0 o0 :: ons;
              md_sigs   = sl;}
  | _ -> assert false

let close_merge_module md =
  match md.md_in_ns, md.md_out_ns, md.md_prefix, md.md_sigs with
  | _ :: i1 :: ins, o0 :: o1 :: ons, p0 :: pl, _ :: sl ->
     let i1, o1 = merge_ns o0 i1, merge_ns o0 o1 in
     let i1, o1 = ns_add_ns i1 p0 o0, ns_add_ns o1 p0 o0 in
     {md with md_prefix = pl;
              md_in_ns  = i1 :: ins;
              md_out_ns = o1 :: ons;
              md_sigs   = sl}
  | _ -> assert false

let wrap_up_module md = match md.md_sigs with
  | [s] -> {md with md_sigs = [List.rev s]}
  | _ -> assert false

let get_top_sigs md = match md.md_sigs with
  | s0 :: _ -> List.rev s0
  | _ -> assert false

let get_top_in_ns md = match md.md_in_ns with
  | i0 :: _ -> i0
  | _ -> assert false

(* let add_ns_to_md ns md =
 *   let md_in_ns = join_ns md.md_in_ns ns in
 *   { md with md_in_ns } *)

(* let add_md f kid ns md =
 *   let md = add_ns_to_md ns md in
 *   let md = {md with md_in_ns = ns_add_ns md.md_in_ns f ns} in
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

(** Pretty printing *)

open Opprintast

let rec ns_names nsm =
  List.map fst (Mstr.bindings nsm)

let print_mstr_vals printer fmt m =
  let print_elem e = pp fmt "@[%a@]@\n" printer e in
  Mstr.iter (fun _ -> print_elem) m

let print_ns fmt {ns_ts;ns_ls;ns_xs;ns_ns} =
  pp fmt "@[Type symbols@\n%a@\nLogic Symbols@\n%a@\nException \
          Symbols@\n%a@\nNamespaces@\n%a@.@]"
    (print_mstr_vals print_ts) ns_ts
    (print_mstr_vals print_ls_decl) ns_ls
    (print_mstr_vals print_xs) ns_xs
    (list ~sep:"\n" constant_string) (ns_names ns_ns)

let rec print_mod fmt {md_nm;md_sigs;md_out_ns} =
  match md_out_ns, md_sigs with
  | o0 :: _, s0 :: _ ->
     pp fmt "@[module %a\nNamespace@\n@[<h 2>@\n%a@]\nSignatures@\n@[<h 2>@\n%a@]@]"
       print_ident md_nm
       print_ns o0
       print_signature s0
  | _ -> assert false
