(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

open Ppxlib
open Utils
open Parsetree
open Uast
open Uast_utils

let gospel = "gospel"

let has_prefix ~prefix:p s =
  let l = String.length p in
  String.length s >= l && String.sub s 0 l = p

let is_spec attr = has_prefix ~prefix:gospel attr.attr_name.txt
let is_type_spec = function | Stype _ -> true | _ -> false
let is_val_spec  = function | Sval _  -> true | _ -> false
let is_func_spec = function | Sfunc_spec _ -> true | _ -> false

let get_attr_content attr = match attr.attr_payload with
  | PStr
      [{pstr_desc =
          Pstr_eval
            ({pexp_desc = Pexp_constant (Pconst_string (spec, _, _))},
             _)}] -> spec, attr.attr_loc
        | _ -> assert false

let get_type_spec = function
  | Stype (x,_) -> x | _ -> assert false

let get_val_spec = function
  | Sval (x,_) -> x | _ -> assert false

let get_func_spec = function
  | Sfunc_spec (s,_) -> s | _ -> assert false

let split_attr attrs = List.partition is_spec attrs

(** An iterator to check if there are attributes that are GOSPEL
   specification. A warning is printed for each one that is found. *)
let unsupported = object
  inherit Ast_traverse.iter

  method! attribute attr =
    if is_spec attr then
      Fmt.epr "@[%a@\n@{<warning>Warning:@} Specification not supported@]@."
        Location.print attr.attr_loc
end

exception Syntax_error of Location.t
exception Floating_not_allowed of Location.t
exception Orphan_decl_spec of Location.t

let () =
  let open Location.Error in
  register_error_of_exn (function
      | Syntax_error loc ->
        Fmt.kstr (fun str -> Some (make ~loc ~sub:[] str))
          "syntax error"
      | Floating_not_allowed loc ->
        Fmt.kstr (fun str -> Some (make ~loc ~sub:[] str))
          "floating specification not allowed"
      | Orphan_decl_spec loc ->
        Fmt.kstr (fun str -> Some (make ~loc ~sub:[] str))
          "orphan specification"
      | _ -> None )

(** Parses the attribute content using the specification
   parser. Raises Syntax_error if syntax errors are found, and
   Ghost_decl if a signature starts with VAL or TYPE: in this case,
   the OCaml parser should be used to parse the signature. *)
let parse_gospel attr =
  let spec,loc = get_attr_content attr in
  let lb = Lexing.from_string spec in
  let open Location in
  let open Lexing in
  init lb loc.loc_start.pos_fname;
  lb.lex_curr_p  <- loc.loc_start;
  lb.lex_abs_pos <- loc.loc_start.pos_cnum;
  try Uparser.spec_init Ulexer.token lb with
    Uparser.Error -> begin
      let loc_start,loc_end = lb.lex_start_p, lb.lex_curr_p in
      let loc = Location.{loc_start; loc_end; loc_ghost=false}  in
      raise (Syntax_error loc) end

(** Calls the OCaml interface parser on the content of the
   attribute. It fails if the OCaml parser parses something that is
   not a type or a val. *)
let ghost_spec attr =
  let spec,loc = get_attr_content attr in
  let lb = Lexing.from_string spec in
  let open Location in
  let open Lexing in
  init lb loc.loc_start.pos_fname;
  lb.lex_curr_p <- loc.loc_start;
  lb.lex_abs_pos <- loc.loc_start.pos_cnum;
  let sign =
    try Parser.interface Lexer.token lb with
      Parser.Error -> begin
        let loc_start,loc_end = lb.lex_start_p, lb.lex_curr_p in
        let loc = Location.{loc_start; loc_end; loc_ghost=false}  in
        raise (Syntax_error loc) end in
  match sign with
  | [{psig_desc = (Psig_type (r,td));psig_loc}] ->
     Stype_ghost (r,td,psig_loc)
  | [{psig_desc = (Psig_value vd);psig_loc}] ->
     Sval_ghost (vd,psig_loc)
  | [{psig_desc = (Psig_open od);psig_loc}] ->
     Sopen_ghost (od,psig_loc)
  | _  (* should not happen *)               -> assert false


(** Tries to apply the specification parser and if the parser raises a
   Ghost_decl exception, it tries the OCaml interface parser *)
let attr2spec a = try parse_gospel a with
                  | Ghost_decl -> ghost_spec a

(** It parses the attributes attached to a type declaration and
   returns a new type declaration with a specification and also the
   part of the specification that could not be attached to the type
   declaration (they are probably floating specification). If
   [extra_spec] is provided they are merged with the declaration
   specification. *)
let type_spec ?(extra_spec=[]) t =
  (* no specification attached to unsupported fields *)
  List.iter (fun (c, _) -> unsupported#core_type c) t.ptype_params;
  List.iter
    (fun (c1, c2, _) -> unsupported#core_type c1; unsupported#core_type c2)
    t.ptype_cstrs;
  unsupported#type_kind t.ptype_kind;
  Option.iter unsupported#core_type t.ptype_manifest;

  let spec,attr = split_attr t.ptype_attributes in
  let spec = List.map attr2spec spec in

  let tspec,fspec = Utils.split_at_f is_type_spec spec in
  let tspec = List.map get_type_spec tspec in
  let tspec = tspec @ extra_spec in
  let tspec = List.fold_left tspec_union empty_tspec tspec in
  let td = { tname = t.ptype_name;       tparams= t.ptype_params;
             tcstrs = t.ptype_cstrs;     tkind = t.ptype_kind;
             tprivate = t.ptype_private; tmanifest = t.ptype_manifest;
             tattributes = attr;         tspec = tspec;
             tloc = t.ptype_loc;} in
  td, fspec

(** It parses a list of type declarations. If more than one item is
   presented only the last one can have attributes that correspond to
   floating specification. [extra_spec], if provided, is appended to
   the last type declaration specification. Raises
   Floating_not_allowed if floating specification is found in the
   middle of recursive type declaration;*)
let type_declaration ?(extra_spec=[]) t =
  (* when we have a recursive type, we only allow floating spec
     attributes in the last element *)
  let rec get_tspecs = function
  | [] -> [],[]
  | [t] ->
     let td,fspec = type_spec ~extra_spec t in
     [td],fspec
  | t::ts ->
     let td,fspec = type_spec t in
     if fspec != [] then raise (Floating_not_allowed t.ptype_loc);
     let tds,fspec = get_tspecs ts in
     td::tds,fspec in
  let td,fspec = get_tspecs t in
  td, fspec

(** It parses the attributes of a val description. Only the first
   attribute is taken into account for the val specification. All
   other are assumed to be floating specification. *)
let val_description v =
  (* no specification attached to unsupported fields *)
  unsupported#core_type v.pval_type;

  let spec,attrs =  split_attr v.pval_attributes in
  let spec = List.map attr2spec spec in

  let vd =
    { vname = v.pval_name; vtype = v.pval_type; vprim = v.pval_prim;
      vattributes = attrs; vspec = None;        vloc = v.pval_loc;} in

  match spec with
  | [] -> vd, spec
  | x::xs when is_val_spec x ->
     { vd with vspec = Some (get_val_spec x)}, xs
  | xs -> vd, xs

(** It parses floating attributes for specification. If nested
   specification is found in type/val declarations they must be
   type/val specification.

   Raises (1) Floating_not_allowed if nested specification is a
   floating specification; (2) Orphan_decl_spec if floating
   specification is a type declaration or val description*)
let rec floating_specs = function
  | [] -> []
  | Sopen_ghost (od,sloc) :: xs ->
     {sdesc=Sig_ghost_open od; sloc} :: floating_specs xs
  | Sfunction (f,sloc) :: xs ->
     (* Look forward and get floating function specification *)
     let (fun_specs,xs) = split_at_f is_func_spec xs in
     let fun_specs = List.map get_func_spec fun_specs in
     let fun_specs = List.fold_left Uast_utils.fspec_union
                     f.fun_spec fun_specs in
     let f = {f with fun_spec = fun_specs } in
     {sdesc=Sig_function f;sloc} :: floating_specs xs
  | Saxiom (a,sloc) :: xs ->
     {sdesc=Sig_axiom a;sloc} :: floating_specs xs
  | Stype_ghost (r,td,sloc) :: xs ->
     (* Look forward and get floating type specification *)
     let tspecs,xs = split_at_f is_type_spec xs in
     let extra_spec = List.map get_type_spec tspecs in
     let td,fspec = type_declaration ~extra_spec td in
     (* if there is nested specification they must refer to the ghost type *)
     if fspec != [] then
       raise (Floating_not_allowed sloc);
     let sdesc = Sig_ghost_type (r,td) in
     {sdesc;sloc} :: floating_specs xs
  | Sval_ghost (vd,sloc) :: xs ->
     let vd,fspec = val_description vd in
     (* if there is nested specification they must refer to the ghost val *)
     if fspec != [] then
       raise (Floating_not_allowed sloc);
     let vd,xs =
       if vd.vspec = None then
         (* val spec might be in the subsequent floating specs *)
         match xs with
         | Sval (vs,_) :: xs -> {vd with vspec=Some vs}, xs
         | _ -> vd, xs
       else (* this val already contains a spec *)
         vd, xs in

     let sdesc = Sig_ghost_val vd in
     {sdesc;sloc} :: floating_specs xs
  | Stype (_,loc) :: _ -> raise (Orphan_decl_spec loc)
  | Sval (_,loc)  :: _ -> raise (Orphan_decl_spec loc)
  | Sfunc_spec (_,loc) :: _ -> raise (Orphan_decl_spec loc)

(** Raises warning if specifications are found in inner attributes and
   simply creates a s_with_constraint. *)
let with_constraint c =
  unsupported#with_constraint c;

  let no_spec_type_decl t =
    { tname = t.ptype_name; tparams = t.ptype_params;
      tcstrs = t.ptype_cstrs; tkind = t.ptype_kind;
      tprivate = t.ptype_private; tmanifest = t.ptype_manifest;
      tattributes = t.ptype_attributes;
      tspec = empty_tspec; tloc = t.ptype_loc;}
  in match c with
  | Pwith_type (l,t) -> Wtype (l,no_spec_type_decl t)
  | Pwith_module (l1,l2) -> Wmodule (l1,l2)
  | Pwith_typesubst (l,t) -> Wtypesubst (l,no_spec_type_decl t)
  | Pwith_modsubst (l1,l2) -> Wmodsubst (l1,l2)


let get_spec_attrs attrs =
  let specs,attrs = split_attr attrs in
  attrs, floating_specs (List.map attr2spec specs)

(** Translats OCaml signatures with specification attached to
   attributes into our intermediate representation. Beaware,
   prev_floats must be reverted before used *)
let rec signature_ sigs acc prev_floats = match sigs with
  | [] -> acc @ floating_specs (List.rev prev_floats)
  | {psig_desc=Psig_attribute a; _} :: xs  when (is_spec a) ->
     (* in this special case, we put together all the floating specs
        and only when seing another signature convert them into
        specification *)
     signature_ xs acc (attr2spec a :: prev_floats)
  | {psig_desc;psig_loc=sloc} :: xs ->
     let prev_specs = floating_specs (List.rev prev_floats) in
     let current_specs = match psig_desc with
       | Psig_value v ->
          let vd,fspec = val_description v in
          let current = {sdesc=Sig_val vd;sloc} in
          let attached = floating_specs fspec in
          current :: attached
       | Psig_type (r,t) ->
          let td,fspec = type_declaration t in
          let current = {sdesc=Sig_type (r,td);sloc} in
          let attached = floating_specs fspec in
          current :: attached
       | Psig_attribute a ->
          [{sdesc=Sig_attribute a;sloc}]
       | Psig_module m ->
          let md, spec = module_declaration m in
          {sdesc=Sig_module md;sloc} :: spec
       | Psig_recmodule d ->
          let mds,spec = List.fold_right (fun m (mds,specs) ->
                             let md, spec = module_declaration m in
                             (md::mds,spec @ specs)
                           ) d ([],[]) in
          {sdesc=Sig_recmodule mds;sloc} :: spec
       | Psig_modtype d ->
          let m, spec = module_type_declaration d in
          {sdesc=Sig_modtype m;sloc} :: spec
       | Psig_typext t ->
          let attrs, _ = get_spec_attrs t.ptyext_attributes in
          let t = {t with ptyext_attributes = attrs} in
          unsupported#type_extension t;
          [{sdesc=Sig_typext t;sloc}]
       | Psig_exception e ->
          let attrs,specs = get_spec_attrs e.ptyexn_attributes in
          let e = {e with ptyexn_attributes = attrs} in
          unsupported#type_exception e;
          let current = {sdesc=Sig_exception e;sloc} in
          current :: specs
       | Psig_open o ->
          let attrs,specs = get_spec_attrs o.popen_attributes in
          let o = {o with popen_attributes = attrs } in
          unsupported#open_description o;
          {sdesc=Sig_open o;sloc} :: specs
       | Psig_include i ->
          let attrs, _ = get_spec_attrs i.pincl_attributes in
          let i = {i with pincl_attributes = attrs} in
          unsupported#include_description i;
          [{sdesc=Sig_include i;sloc}]
       | Psig_class c ->
          let c,specs =
            List.fold_right (fun cd (cl,specl) ->
                let attrs,specs = get_spec_attrs cd.pci_attributes in
                let c = {cd with pci_attributes = attrs} in
                unsupported#class_description c;
                c::cl, specs @ specl
              ) c ([],[]) in
          {sdesc=Sig_class c;sloc} :: specs
       | Psig_class_type c ->
          let c, _ =
            List.fold_right (fun cd (cl,specl) ->
                let attrs,specs = get_spec_attrs cd.pci_attributes in
                let c = {cd with pci_attributes = attrs} in
                unsupported#class_type_declaration c;
                c::cl, specs @ specl
              ) c ([],[]) in
          [{sdesc=Sig_class_type c;sloc}]
       | Psig_extension (e,a) ->
          let attrs,specs = get_spec_attrs a in
          {sdesc=Sig_extension (e,attrs);sloc} :: specs
       | Psig_typesubst _ | Psig_modsubst _ ->
         assert false (* TODO(@pascutto) *)
     in
     let all_specs = acc @ prev_specs @ current_specs in
     signature_ xs all_specs []

and signature sigs = signature_ sigs [] []

and module_type_desc m =
  match m with
  | Pmty_ident id ->
     Mod_ident id
  | Pmty_signature s ->
     Mod_signature (signature s)
  | Pmty_functor (fp, mt) ->
     Mod_functor (functor_parameter fp, module_type mt)
  | Pmty_with (m,c) ->
     Mod_with (module_type m, List.map with_constraint c)
  | Pmty_typeof m ->
     unsupported#module_expr m; Mod_typeof m
  | Pmty_extension e -> Mod_extension e
  | Pmty_alias a -> Mod_alias a

and functor_parameter = function
  | Unit -> Unit
  | Named (s, m) -> Named (s, module_type m)

and module_type m =
  unsupported#attributes m.pmty_attributes;
  { mdesc = module_type_desc m.pmty_desc;
    mloc = m.pmty_loc; mattributes = m.pmty_attributes}

and module_declaration m =
  let attrs, specs = get_spec_attrs m.pmd_attributes in
  let m = { mdname = m.pmd_name; mdtype = module_type m.pmd_type;
    mdattributes = attrs; mdloc = m.pmd_loc } in
  m, specs

and module_type_declaration m =
  let attrs, specs = get_spec_attrs m.pmtd_attributes in
  let mtd = { mtdname = m.pmtd_name;
              mtdtype = Option.map module_type m.pmtd_type;
              mtdattributes = attrs; mtdloc = m.pmtd_loc} in
  mtd, specs
