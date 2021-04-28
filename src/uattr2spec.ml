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

let parse_gospel parse attr =
  let spec, loc = get_spec_content attr in
  let lb = Lexing.from_string spec in
  try spec, parse Ulexer.token lb with Uparser.Error -> raise (Syntax_error loc)

let type_declaration t =
  let spec_attr, other_attrs = get_spec_attr t.ptype_attributes in
  let parse attr = snd (parse_gospel Uparser.type_spec attr) in
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

let val_description v =
  let spec_attr, other_attrs = get_spec_attr v.pval_attributes in
  let parse attr =
    let text, spec = parse_gospel Uparser.val_spec attr in
    { spec with sp_text = text } in
  let spec = Option.map parse spec_attr in
  {
    vname = v.pval_name;
    vtype = v.pval_type;
    vprim = v.pval_prim;
    vattributes = other_attrs;
    vspec = spec;
    vloc = v.pval_loc;
  }

let ghost_spec attr =
  let spec, loc = get_spec_content attr in
  let lb = Lexing.from_string spec in
  try
    Parser.interface Lexer.token lb |> function
    | [ { psig_desc = Psig_type (r, [ t ]); _ } ] ->
        let type_ = type_declaration t in
        if type_.tspec = None then
          let tspec =
            get_inner_spec attr |> fst
            |> Option.map (parse_gospel Uparser.type_spec)
            |> Option.map snd (* FIXME *)
          in
          Sig_ghost_type (r, [ { type_ with tspec } ])
        else Sig_ghost_type (r, [ type_ ])
    | [ { psig_desc = Psig_value vd; _ } ] ->
        let val_ = val_description vd in
        if val_.vspec = None then
          let vspec =
            get_inner_spec attr |> fst
            |> Option.map (parse_gospel Uparser.val_spec)
            |> Option.map snd (* FIXME *)
          in
          Sig_ghost_val { val_ with vspec }
        else Sig_ghost_val val_
    | [ { psig_desc = Psig_open od; _ } ] -> Sig_ghost_open od
    | _ -> assert false
  with Parser.Error -> raise (Syntax_error loc)

let floating_spec a =
  try
    let _, fun_ = parse_gospel Uparser.func a in (* FIXME *)
    if fun_.fun_spec = None then
      let fun_spec =
        get_inner_spec a |> fst |> Option.map (parse_gospel Uparser.func_spec)
        |> Option.map snd (* FIXME *)
      in
      Sig_function { fun_ with fun_spec }
    else Sig_function fun_
  with Syntax_error _ -> (
    try Sig_axiom (snd (parse_gospel Uparser.axiom a)) (* FIXME *)
    with Syntax_error _ -> ghost_spec a)

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

let rec signature_item_desc = function
  | Psig_value v -> Sig_val (val_description v)
  | Psig_type (r, tl) -> Sig_type (r, List.map type_declaration tl)
  | Psig_attribute a ->
      if not (is_spec a) then Sig_attribute a else floating_spec a
  | Psig_module m -> Sig_module (module_declaration m)
  | Psig_recmodule d -> Sig_recmodule (List.map module_declaration d)
  | Psig_modtype d -> Sig_modtype (module_type_declaration d)
  | Psig_typext t -> Sig_typext t
  | Psig_exception e -> Sig_exception e
  | Psig_open o -> Sig_open o
  | Psig_include i -> Sig_include i
  | Psig_class c -> Sig_class c
  | Psig_class_type c -> Sig_class_type c
  | Psig_extension (e, a) -> Sig_extension (e, a)
  | Psig_typesubst _ | Psig_modsubst _ -> assert false

(* TODO(@pascutto) *)
and signature sigs =
  List.map
    (fun { psig_desc; psig_loc } ->
      { sdesc = signature_item_desc psig_desc; sloc = psig_loc })
    sigs

and module_type_desc = function
  | Pmty_ident id -> Mod_ident id
  | Pmty_signature s -> Mod_signature (signature s)
  | Pmty_functor (fp, mt) -> Mod_functor (functor_parameter fp, module_type mt)
  | Pmty_with (m, c) -> Mod_with (module_type m, List.map with_constraint c)
  | Pmty_typeof m -> Mod_typeof m
  | Pmty_extension e -> Mod_extension e
  | Pmty_alias a -> Mod_alias a

and functor_parameter = function
  | Unit -> Unit
  | Named (s, m) -> Named (s, module_type m)

and module_type m =
  {
    mdesc = module_type_desc m.pmty_desc;
    mloc = m.pmty_loc;
    mattributes = m.pmty_attributes;
  }

and module_declaration m =
  {
    mdname = m.pmd_name;
    mdtype = module_type m.pmd_type;
    mdattributes = m.pmd_attributes;
    mdloc = m.pmd_loc;
  }

and module_type_declaration m =
  {
    mtdname = m.pmtd_name;
    mtdtype = Option.map module_type m.pmtd_type;
    mtdattributes = m.pmtd_attributes;
    mtdloc = m.pmtd_loc;
  }
