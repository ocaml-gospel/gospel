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
open Parse_uast
open Uast_utils

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
              ( {
                  pexp_desc = Pexp_constant (Pconst_string (spec, spec_loc, _));
                  _;
                },
                _ );
          _;
        };
      ] ->
      (spec, spec_loc)
  | _ -> assert false

let get_spec_loc attr = snd (get_spec_content attr)

let parse_gospel ~filename parse attr =
  let spec, spec_loc = get_spec_content attr in
  let lb = Lexing.from_string spec in
  Lexing.set_position lb spec_loc.loc_start;
  Lexing.set_filename lb filename;
  try (spec, parse Ulexer.token lb)
  with Uparser.Error ->
    let loc =
      { loc_start = lb.lex_start_p; loc_end = lb.lex_curr_p; loc_ghost = false }
    in
    W.error ~loc W.Syntax_error

exception OCaml_unsupported
exception OCaml_unsupported_and_annotated of (Location.t * string)

let ocaml_unsupported ~annotated ~loc str =
  if annotated then raise @@ OCaml_unsupported_and_annotated (loc, str)
  else raise OCaml_unsupported

let params_to_id =
  let param_to_id (core_type, _) =
    let loc = core_type.ptyp_loc in
    match core_type.ptyp_desc with
    | Ptyp_var s -> Preid.create s ~loc
    | Ptyp_any -> raise OCaml_unsupported
    | _ -> assert false
    (* There are no other possible values for type parameters in type declarations *)
  in
  List.map param_to_id

let preid_of_loc s = Preid.create ~loc:s.loc s.txt

let rec preid_of_long (loc : location) (s : longident) =
  let preid_of_long = preid_of_long loc in
  match s with
  | Lident id -> Qid (Preid.create ~loc id)
  | Ldot (id, s) -> Qdot (preid_of_long id, Preid.create ~loc s)
  | _ -> assert false

let rec core_to_pty ~annotated cty =
  let loc = cty.ptyp_loc in
  match cty.ptyp_desc with
  | Ptyp_var str -> PTtyvar (Preid.create ~loc str)
  | Ptyp_constr (id, l) ->
      PTtyapp (preid_of_long id.loc id.txt, List.map (core_to_pty ~annotated) l)
  | Ptyp_arrow (_, t1, t2) ->
      PTarrow (core_to_pty ~annotated t1, core_to_pty ~annotated t2)
  | Ptyp_tuple l -> PTtuple (List.map (core_to_pty ~annotated) l)
  | Ptyp_any -> ocaml_unsupported ~annotated ~loc "anonymous type"
  | Ptyp_object (_, _) -> ocaml_unsupported ~annotated ~loc "object"
  | Ptyp_class (_, _) -> ocaml_unsupported ~annotated ~loc "class"
  | Ptyp_alias (_, _) -> ocaml_unsupported ~annotated ~loc "alias"
  | Ptyp_variant (_, _, _) ->
      ocaml_unsupported ~annotated ~loc "polymorphic variant"
  | Ptyp_poly (_, _) ->
      ocaml_unsupported ~annotated ~loc "polymorphic type variable"
  | Ptyp_package _ -> ocaml_unsupported ~annotated ~loc "first class module"
  | Ptyp_open (_, _) -> ocaml_unsupported ~annotated ~loc "local open"
  | Ptyp_extension _ -> ocaml_unsupported ~annotated ~loc "type extension"

let ptype_kind ~annotated ~loc = function
  | Ptype_abstract -> PTtype_abstract
  | Ptype_record l ->
      let to_gospel_label l =
        {
          pld_name = preid_of_loc l.Ppxlib.pld_name;
          pld_mutable =
            (match l.pld_mutable with
            | Mutable -> Mutable
            | Immutable -> Immutable);
          pld_type = core_to_pty ~annotated l.pld_type;
          pld_loc = l.pld_loc;
        }
      in
      PTtype_record (List.map to_gospel_label l)
  | Ptype_variant _ -> ocaml_unsupported ~annotated ~loc "variant"
  | Ptype_open -> ocaml_unsupported ~annotated ~loc "extensible type"

let mk_tdecl t tkind attrs spec =
  let tparams = params_to_id t.ptype_params
  and annotated = Option.is_some spec in
  {
    tname = preid_of_loc t.ptype_name;
    tparams;
    tkind;
    tmanifest = Option.map (core_to_pty ~annotated) t.ptype_manifest;
    tattributes = attrs;
    tspec = spec;
    tloc = t.ptype_loc;
  }

let type_declaration ~filename t =
  let spec_attr, other_attrs = get_spec_attr t.ptype_attributes in
  let annotated = Option.is_some spec_attr in
  let tkind = ptype_kind ~annotated ~loc:t.ptype_loc t.ptype_kind in
  let parse attr =
    let ty_text, spec = parse_gospel ~filename Uparser.type_spec attr in
    let ty_loc = get_spec_loc attr in
    { spec with ty_text; ty_loc }
  in
  let spec = Option.map parse spec_attr in
  mk_tdecl t tkind other_attrs spec

let val_description ~filename v =
  let spec_attr, other_attrs = get_spec_attr v.pval_attributes in
  let parse attr =
    let sp_text, spec = parse_gospel ~filename Uparser.val_spec attr in
    let sp_loc = get_spec_loc attr in
    { spec with sp_text; sp_loc }
  in
  let spec = Option.map parse spec_attr in
  let annotated = Option.is_some spec in
  {
    vname = preid_of_loc v.pval_name;
    vtype = core_to_pty ~annotated v.pval_type;
    vattributes = other_attrs;
    vspec = spec;
    vloc = v.pval_loc;
  }

let floating_spec ~filename a =
  let txt, s = parse_gospel ~filename Uparser.top a in
  Sig_gospel (s, txt)

let sig_exception exn =
  let c = exn.ptyexn_constructor in
  let exn_id = preid_of_loc c.pext_name in
  let exn_loc = c.pext_loc in
  let exn_attributes = c.pext_attributes in
  let exn_args =
    match c.pext_kind with
    | Pext_decl ([], Pcstr_tuple args, _) ->
        List.map (core_to_pty ~annotated:false) args
    | Pext_rebind _ -> assert false (* Cannot occur on an interface file. *)
    | _ -> assert false
  in
  { exn_id; exn_loc; exn_attributes; exn_args }

(** [signature_item_desc ~filename s] turns the OCaml signature [s] into an
    appropriate Gospel signature. *)
let rec signature_item_desc ~filename = function
  | Psig_value v as s -> (
      try Sig_val (val_description ~filename v) with
      | OCaml_unsupported -> Sig_unsupported s
      | OCaml_unsupported_and_annotated (loc, str) -> W.unsupported ~loc str)
  | Psig_type (_, tl) as s -> (
      try Sig_type (List.map (type_declaration ~filename) tl) with
      | OCaml_unsupported -> Sig_unsupported s
      | OCaml_unsupported_and_annotated (loc, str) -> W.unsupported ~loc str)
  | Psig_attribute a ->
      if not (is_spec a) then Sig_attribute a else floating_spec ~filename a
  | Psig_module m as s -> (
      match module_declaration ~filename m with
      | None -> Sig_unsupported s
      | Some decl -> Sig_module decl)
  | Psig_exception e as s -> (
      try Sig_exception (sig_exception e) with
      | OCaml_unsupported -> Sig_unsupported s
      | OCaml_unsupported_and_annotated (loc, str) -> W.unsupported ~loc str)
  (* Unsupported *)
  | Psig_recmodule _ as s -> Sig_unsupported s
  | Psig_modtype _ as s -> Sig_unsupported s
  | Psig_typext _ as s -> Sig_unsupported s
  | Psig_open _ as s -> Sig_unsupported s
  | Psig_include _ as s -> Sig_unsupported s
  | Psig_class _ as s -> Sig_unsupported s
  | Psig_class_type _ as s -> Sig_unsupported s
  | Psig_extension _ as s -> Sig_unsupported s
  | Psig_typesubst _ as s -> Sig_unsupported s
  | Psig_modsubst _ as s -> Sig_unsupported s
  | Psig_modtypesubst _ as s -> Sig_unsupported s

and signature ~filename sigs =
  List.map
    (fun { psig_desc; psig_loc } ->
      let filename =
        match psig_loc.loc_start.pos_fname with
        | "" | "_none_" -> filename
        | f -> f
      in
      let sdesc = signature_item_desc ~filename psig_desc in
      { sdesc; sloc = psig_loc })
    sigs

and module_type_desc ~filename = function
  | Pmty_signature s -> Some (Mod_signature (signature ~filename s))
  | Pmty_ident _ -> None
  | Pmty_functor _ -> None
  | Pmty_with _ -> None
  | Pmty_typeof _ -> None
  | Pmty_extension _ -> None
  | Pmty_alias _ -> None

and module_type ~filename m =
  let* mdesc = module_type_desc ~filename m.pmty_desc in
  { mdesc; mloc = m.pmty_loc; mattributes = m.pmty_attributes }

and module_declaration ~filename m =
  let loc = m.pmd_name.loc in
  let* nm = m.pmd_name.txt and* mdtype = module_type ~filename m.pmd_type in
  {
    mdname = Preid.create ~loc nm;
    mdtype;
    mdattributes = m.pmd_attributes;
    mdloc = m.pmd_loc;
  }
