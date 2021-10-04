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

let is_spec attr = attr.attr_name.txt = "gospel"

let rec get_spec_attr = function
  | [] -> (None, [])
  | h :: t when is_spec h -> (Some h, t)
  | h :: t ->
      let elt, rest = get_spec_attr t in
      (elt, h :: rest)

let get_spec_content attr =
  match attr.attr_payload with
  | PStr
      [
        {
          pstr_desc =
            Pstr_eval
              ({ pexp_desc = Pexp_constant (Pconst_string (spec, _, _)) }, _);
        };
      ] ->
      (spec, attr.attr_loc)
  | _ -> assert false

let get_inner_spec attr =
  match attr.attr_payload with
  | PStr [ { pstr_desc = Pstr_eval (_, attrs) } ] -> get_spec_attr attrs
  | _ -> assert false

exception Syntax_error of Location.t

let () =
  let open Location.Error in
  register_error_of_exn (function
    | Syntax_error loc ->
        Fmt.kstr (fun str -> Some (make ~loc ~sub:[] str)) "syntax error"
    | _ -> None)

(* XXX: Use Lexing.set_position when moving to OCaml 4.11 *)
let set_position (lexbuf : Lexing.lexbuf) (position : Lexing.position) =
  lexbuf.lex_curr_p <- { position with pos_fname = lexbuf.lex_curr_p.pos_fname };
  lexbuf.lex_abs_pos <- position.pos_cnum

(* XXX: Use Lexing.set_filename when moving to OCaml 4.11 *)
let set_filename (lexbuf : Lexing.lexbuf) (fname : string) =
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = fname }

let parse_gospel ~filename parse attr =
  let spec, loc = get_spec_content attr in
  let lb = Lexing.from_string spec in
  set_position lb attr.attr_loc.loc_start;
  set_filename lb filename;
  try (spec, parse Ulexer.token lb)
  with Uparser.Error -> raise (Syntax_error loc)

let type_declaration ~filename t =
  let spec_attr, other_attrs = get_spec_attr t.ptype_attributes in
  let parse attr = snd (parse_gospel ~filename Uparser.type_spec attr) in
  let spec = Option.map parse spec_attr in
  {
    tname = t.ptype_name;
    tparams = t.ptype_params;
    tcstrs = t.ptype_cstrs;
    tkind = t.ptype_kind;
    tprivate = t.ptype_private;
    tmanifest = t.ptype_manifest;
    tattributes = other_attrs;
    tspec = spec;
    tloc = t.ptype_loc;
  }

let val_description ~filename v =
  let spec_attr, other_attrs = get_spec_attr v.pval_attributes in
  let parse attr =
    let text, spec = parse_gospel ~filename Uparser.val_spec attr in
    { spec with sp_text = text; sp_loc = attr.attr_loc }
  in
  let spec = Option.map parse spec_attr in
  {
    vname = v.pval_name;
    vtype = v.pval_type;
    vprim = v.pval_prim;
    vattributes = other_attrs;
    vspec = spec;
    vloc = v.pval_loc;
  }

let ghost_spec ~filename attr =
  let spec, loc = get_spec_content attr in
  let lb = Lexing.from_string spec in
  let sigs = try Parse.interface lb with _ -> raise (Syntax_error loc) in
  match sigs with
  | [ { psig_desc = Psig_type (r, [ t ]); _ } ] ->
      let type_ = type_declaration ~filename t in
      if type_.tspec = None then
        let tspec =
          get_inner_spec attr
          |> fst
          |> Option.map (parse_gospel ~filename Uparser.type_spec)
          |> Option.map snd
          (* FIXME *)
        in
        Sig_ghost_type (r, [ { type_ with tspec } ])
      else Sig_ghost_type (r, [ type_ ])
  | [ { psig_desc = Psig_value vd; _ } ] ->
      let val_ = val_description ~filename vd in
      if val_.vspec = None then
        let vspec =
          get_inner_spec attr
          |> fst
          |> Option.map (parse_gospel ~filename Uparser.val_spec)
          |> Option.map snd
          (* FIXME *)
        in
        Sig_ghost_val { val_ with vspec }
      else Sig_ghost_val val_
  | [ { psig_desc = Psig_open od; _ } ] -> Sig_ghost_open od
  | _ -> assert false

let floating_spec ~filename a =
  try
    let _, fun_ = parse_gospel ~filename Uparser.func a in
    (* FIXME *)
    if fun_.fun_spec = None then
      let fun_spec =
        get_inner_spec a
        |> fst
        |> Option.map (parse_gospel ~filename Uparser.func_spec)
        |> Option.map snd
        (* FIXME *)
      in
      Sig_function { fun_ with fun_spec }
    else Sig_function fun_
  with Syntax_error _ -> (
    try Sig_axiom (snd (parse_gospel ~filename Uparser.axiom a)) (* FIXME *)
    with Syntax_error _ -> ghost_spec ~filename a)

let with_constraint c =
  let no_spec_type_decl t =
    {
      tname = t.ptype_name;
      tparams = t.ptype_params;
      tcstrs = t.ptype_cstrs;
      tkind = t.ptype_kind;
      tprivate = t.ptype_private;
      tmanifest = t.ptype_manifest;
      tattributes = t.ptype_attributes;
      tspec = None;
      tloc = t.ptype_loc;
    }
  in
  match c with
  | Pwith_type (l, t) -> Wtype (l, no_spec_type_decl t)
  | Pwith_module (l1, l2) -> Wmodule (l1, l2)
  | Pwith_typesubst (l, t) -> Wtypesubst (l, no_spec_type_decl t)
  | Pwith_modsubst (l1, l2) -> Wmodsubst (l1, l2)

let rec signature_item_desc ~filename = function
  | Psig_value v -> Sig_val (val_description ~filename v)
  | Psig_type (r, tl) -> Sig_type (r, List.map (type_declaration ~filename) tl)
  | Psig_attribute a ->
      if not (is_spec a) then Sig_attribute a else floating_spec ~filename a
  | Psig_module m -> Sig_module (module_declaration ~filename m)
  | Psig_recmodule d ->
      Sig_recmodule (List.map (module_declaration ~filename) d)
  | Psig_modtype d -> Sig_modtype (module_type_declaration ~filename d)
  | Psig_typext t -> Sig_typext t
  | Psig_exception e -> Sig_exception e
  | Psig_open o -> Sig_open o
  | Psig_include i -> Sig_include i
  | Psig_class c -> Sig_class c
  | Psig_class_type c -> Sig_class_type c
  | Psig_extension (e, a) -> Sig_extension (e, a)
  | Psig_typesubst _ | Psig_modsubst _ -> assert false

(* TODO(@pascutto) *)
and signature ~filename sigs =
  List.map
    (fun { psig_desc; psig_loc } ->
      { sdesc = signature_item_desc ~filename psig_desc; sloc = psig_loc })
    sigs

and module_type_desc ~filename = function
  | Pmty_ident id -> Mod_ident id
  | Pmty_signature s -> Mod_signature (signature ~filename s)
  | Pmty_functor (fp, mt) ->
      Mod_functor (functor_parameter ~filename fp, module_type ~filename mt)
  | Pmty_with (m, c) ->
      Mod_with (module_type ~filename m, List.map with_constraint c)
  | Pmty_typeof m -> Mod_typeof m
  | Pmty_extension e -> Mod_extension e
  | Pmty_alias a -> Mod_alias a

and functor_parameter ~filename = function
  | Unit -> Unit
  | Named (s, m) -> Named (s, module_type ~filename m)

and module_type ~filename m =
  {
    mdesc = module_type_desc ~filename m.pmty_desc;
    mloc = m.pmty_loc;
    mattributes = m.pmty_attributes;
  }

and module_declaration ~filename m =
  {
    mdname = m.pmd_name;
    mdtype = module_type ~filename m.pmd_type;
    mdattributes = m.pmd_attributes;
    mdloc = m.pmd_loc;
  }

and module_type_declaration ~filename m =
  {
    mtdname = m.pmtd_name;
    mtdtype = Option.map (module_type ~filename) m.pmtd_type;
    mtdattributes = m.pmtd_attributes;
    mtdloc = m.pmtd_loc;
  }
