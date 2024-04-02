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

let get_inner_spec attr =
  match attr.attr_payload with
  | PStr [ { pstr_desc = Pstr_eval (_, attrs); _ } ] -> get_spec_attr attrs
  | _ -> assert false

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

let type_declaration ~filename t =
  let spec_attr, other_attrs = get_spec_attr t.ptype_attributes in
  let parse attr =
    let ty_text, spec = parse_gospel ~filename Uparser.type_spec attr in
    let ty_loc = get_spec_loc attr in
    { spec with ty_text; ty_loc }
  in
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
    let sp_text, spec = parse_gospel ~filename Uparser.val_spec attr in
    let sp_loc = get_spec_loc attr in
    { spec with sp_text; sp_loc }
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
  Lexing.set_position lb loc.loc_start;
  Lexing.set_filename lb filename;
  let sigs = try Parse.interface lb with _ -> W.error ~loc W.Syntax_error in
  match sigs with
  | [ { psig_desc = Psig_type (r, [ t ]); _ } ] ->
      let type_ = type_declaration ~filename t in
      if type_.tspec = None then
        let tspec =
          get_inner_spec attr
          |> fst
          |> Option.map (parse_gospel ~filename Uparser.type_spec)
          |> Option.map (fun (ty_text, spec) ->
                 let ty_loc = get_spec_loc attr in
                 { spec with ty_text; ty_loc })
        in
        let tloc = get_spec_loc attr in
        Sig_ghost_type (r, [ { type_ with tspec; tloc } ])
      else Sig_ghost_type (r, [ type_ ])
  | [ { psig_desc = Psig_value vd; _ } ] ->
      let val_ = val_description ~filename vd in
      if val_.vspec = None then
        let vspec =
          get_inner_spec attr
          |> fst
          |> Option.map (parse_gospel ~filename Uparser.val_spec)
          |> Option.map (fun (sp_text, spec) ->
                 let sp_loc = get_spec_loc attr in
                 { spec with sp_text; sp_loc })
        in
        let vloc = get_spec_loc attr in
        Sig_ghost_val { val_ with vspec; vloc }
      else Sig_ghost_val val_
  | [ { psig_desc = Psig_open od; _ } ] ->
      let popen_loc = get_spec_loc attr in
      Sig_ghost_open { od with popen_loc }
  | _ -> assert false

let floating_spec ~filename a =
  try
    let fun_text, fun_ = parse_gospel ~filename Uparser.func a in
    let fun_ = { fun_ with fun_text } in
    if fun_.fun_spec = None then
      let fun_spec =
        get_inner_spec a
        |> fst
        |> Option.map (parse_gospel ~filename Uparser.func_spec)
        |> Option.map (fun (fun_text, (spec : fun_spec)) ->
               let fun_loc = get_spec_loc a in
               { spec with fun_text; fun_loc })
      in
      Sig_function { fun_ with fun_spec }
    else Sig_function fun_
  with W.Error (_, W.Syntax_error) -> (
    try
      let ax_text, axiom = parse_gospel ~filename Uparser.axiom a in
      let ax_loc = get_spec_loc a in
      Sig_axiom { axiom with ax_text; ax_loc }
    with W.Error (_, W.Syntax_error) -> (
      try
        let in_text, ind_decl = parse_gospel ~filename Uparser.ind_decl a in
        Sig_inductive { ind_decl with in_text; in_loc = ind_decl.in_loc }
      with W.Error (_, W.Syntax_error) -> ghost_spec ~filename a))

let ghost_spec_str ~filename attr =
  let spec, loc = get_spec_content attr in
  let lb = Lexing.from_string spec in
  Location.init lb loc.loc_start.pos_fname;
  let impls =
    try Parse.implementation lb
    with _ -> raise (W.Error (loc, W.Syntax_error))
  in
  match impls with
  | [ { pstr_desc = Pstr_type (r, [ t ]); _ } ] ->
      let type_ = type_declaration ~filename t in
      if type_.tspec = None then
        let tspec =
          get_inner_spec attr
          |> fst
          |> Option.map (parse_gospel ~filename Uparser.type_spec)
          |> Option.map snd
        in
        Str_ghost_type (r, [ { type_ with tspec } ])
      else Str_ghost_type (r, [ type_ ])
  | [ { pstr_desc = Pstr_primitive vd; _ } ] ->
      let val_ = val_description ~filename vd in
      if val_.vspec = None then
        let vspec =
          get_inner_spec attr
          |> fst
          |> Option.map (parse_gospel ~filename Uparser.val_spec)
          |> Option.map snd
        in
        Str_ghost_val { val_ with vspec }
      else Str_ghost_val val_
  | [ { pstr_desc = Pstr_open od; _ } ] -> Str_ghost_open od
  | _ -> assert false

let floating_spec_str ~filename a =
  try
    let _, fun_ = parse_gospel ~filename Uparser.func a in
    if fun_.fun_spec = None then
      let fun_spec =
        get_inner_spec a
        |> fst
        |> Option.map (parse_gospel ~filename Uparser.func_spec)
        |> Option.map snd
      in
      Str_function { fun_ with fun_spec }
    else Str_function fun_
  with W.Error (_, W.Syntax_error) -> (
    try Str_prop (snd (parse_gospel ~filename Uparser.prop a))
    with W.Error (_, W.Syntax_error) -> (
      try
        let in_text, ind_decl = parse_gospel ~filename Uparser.ind_decl a in
        Str_inductive { ind_decl with in_text; in_loc = ind_decl.in_loc }
      with W.Error (_, W.Syntax_error) -> ghost_spec_str ~filename a))

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
  | Pwith_modtype (l1, l2) -> Wmodtype (l1, l2)
  | Pwith_modtypesubst (l1, l2) -> Wmodtypesubst (l1, l2)

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
  | Psig_typesubst s -> Sig_typesubst (List.map (type_declaration ~filename) s)
  | Psig_modsubst s -> Sig_modsubst s
  | Psig_modtypesubst s ->
      Sig_modtypesubst (module_type_declaration ~filename s)

and signature ~filename sigs =
  List.map
    (fun { psig_desc; psig_loc } ->
      let filename =
        match psig_loc.loc_start.pos_fname with
        | "" | "_none_" -> filename
        | f -> f
      in
      { sdesc = signature_item_desc ~filename psig_desc; sloc = psig_loc })
    sigs

and module_type_desc ~filename spec = function
  | Pmty_ident id -> Mod_ident id
  | Pmty_signature s -> Mod_signature (signature ~filename s)
  | Pmty_functor (fp, mt) ->
      Mod_functor (functor_parameter ~filename fp, module_type ~filename mt)
  | Pmty_with (m, c) ->
      let extra_constraints =
        match spec with
        | None -> []
        | Some c -> snd (parse_gospel ~filename Uparser.with_constraint c)
      in
      let mk_constraint acc c = with_constraint c :: acc in
      let constraints = List.fold_left mk_constraint extra_constraints c in
      Mod_with (module_type ~filename m, constraints)
  | Pmty_typeof m -> Mod_typeof m
  | Pmty_extension e -> Mod_extension e
  | Pmty_alias a -> Mod_alias a

and functor_parameter ~filename = function
  | Unit -> Unit
  | Named (s, m) -> Named (s, module_type ~filename m)

and module_type ~filename m =
  let spec, attr = get_spec_attr m.pmty_attributes in
  {
    mdesc = module_type_desc ~filename spec m.pmty_desc;
    mloc = m.pmty_loc;
    mattributes = attr;
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

and mk_s_structure_item ~loc sstr_desc = { sstr_desc; sstr_loc = loc }

and mk_s_expression spexp_desc spexp_loc spexp_loc_stack spexp_attributes =
  { spexp_desc; spexp_loc; spexp_loc_stack; spexp_attributes }

and mk_s_module_expr spmod_desc spmod_loc spmod_attributes =
  { spmod_desc; spmod_loc; spmod_attributes }

and mk_svb spvb_pat spvb_expr spvb_attributes spvb_vspec spvb_loc =
  { spvb_pat; spvb_expr; spvb_attributes; spvb_vspec; spvb_loc }

and s_expression ~filename expr =
  let s_expression = s_expression ~filename in
  let loc = expr.pexp_loc in
  let loc_stack = expr.pexp_loc_stack in
  let attributes = expr.pexp_attributes in
  let lbl_expr (lbl, expr) = (lbl, s_expression expr) in
  let longid_expr (id, expr) = (id, s_expression expr) in
  let case { pc_lhs; pc_guard; pc_rhs } =
    let spc_lhs = pc_lhs in
    let spc_guard = Option.map s_expression pc_guard in
    let spc_rhs = s_expression pc_rhs in
    { spc_lhs; spc_guard; spc_rhs }
  in
  let spexp_desc = function
    | Pexp_ident id -> Sexp_ident id
    | Pexp_constant c -> Sexp_constant c
    | Pexp_let (rec_flag, vb_list, expr) ->
        let s_vb_list = s_value_binding ~filename vb_list in
        Sexp_let (rec_flag, s_vb_list, s_expression expr)
    | Pexp_function case_list -> Sexp_function (List.map case case_list)
    | Pexp_fun (arg, expr_arg, pat, expr_body) ->
        let spec, _ = get_spec_attr attributes in
        let fun_spec =
          Option.map (parse_gospel ~filename Uparser.func_spec) spec
          |> Option.map snd
        in
        let expr_arg = Option.map s_expression expr_arg in
        let expr_body = s_expression expr_body in
        Sexp_fun (arg, expr_arg, pat, expr_body, fun_spec)
    | Pexp_apply (expr, arg_list) ->
        Sexp_apply (s_expression expr, List.map lbl_expr arg_list)
    | Pexp_match (expr, case_list) ->
        Sexp_match (s_expression expr, List.map case case_list)
    | Pexp_try (expr, case_list) ->
        Sexp_try (s_expression expr, List.map case case_list)
    | Pexp_tuple expr_list -> Sexp_tuple (List.map s_expression expr_list)
    | Pexp_construct (id, expr) ->
        Sexp_construct (id, Option.map s_expression expr)
    | Pexp_variant (label, expr) ->
        Sexp_variant (label, Option.map s_expression expr)
    | Pexp_record (field_list, expression) ->
        let field_list = List.map longid_expr field_list in
        Sexp_record (field_list, Option.map s_expression expression)
    | Pexp_field (expr, id) -> Sexp_field (s_expression expr, id)
    | Pexp_setfield (expr_rec, field, expr_assign) ->
        Sexp_setfield (s_expression expr_rec, field, s_expression expr_assign)
    | Pexp_array expr_list -> Sexp_array (List.map s_expression expr_list)
    | Pexp_ifthenelse (expr1, expr2, expr3) ->
        let expr1 = s_expression expr1 in
        let expr2 = s_expression expr2 in
        Sexp_ifthenelse (expr1, expr2, Option.map s_expression expr3)
    | Pexp_sequence (expr1, expr2) ->
        Sexp_sequence (s_expression expr1, s_expression expr2)
    | Pexp_while (expr1, expr2) ->
        let spec, _ = get_spec_attr attributes in
        let while_spec =
          Option.map (parse_gospel ~filename Uparser.loop_spec) spec
          |> Option.map snd
        in
        Sexp_while (s_expression expr1, s_expression expr2, while_spec)
    | Pexp_for (pat, expr1, expr2, direction_flag, expr3) ->
        let expr1 = s_expression expr1 and expr2 = s_expression expr2 in
        let expr3 = s_expression expr3 in
        (* TODO: avoid all of this code duplication *)
        let spec, _ = get_spec_attr attributes in
        let for_spec =
          Option.map (parse_gospel ~filename Uparser.loop_spec) spec
          |> Option.map snd
        in
        Sexp_for (pat, expr1, expr2, direction_flag, expr3, for_spec)
    | Pexp_constraint (expr, core_type) ->
        Sexp_constraint (s_expression expr, core_type)
    | Pexp_coerce (expr, core_ty_left, core_ty_right) ->
        Sexp_coerce (s_expression expr, core_ty_left, core_ty_right)
    | Pexp_send (expr, label) -> Sexp_send (s_expression expr, label)
    | Pexp_new id -> Sexp_new id
    | Pexp_setinstvar (label, expr) -> Sexp_setinstvar (label, s_expression expr)
    | Pexp_override label_expr_list ->
        let lbl_expr (lbl, expr) = (lbl, s_expression expr) in
        Sexp_override (List.map lbl_expr label_expr_list)
    | Pexp_letmodule (id, mod_expr, expr) ->
        Sexp_letmodule (id, mod_expr, s_expression expr)
    | Pexp_letexception (construct, expr) ->
        Sexp_letexception (construct, s_expression expr)
    | Pexp_assert expr -> Sexp_assert (s_expression expr)
    | Pexp_lazy expr -> Sexp_lazy (s_expression expr)
    | Pexp_poly (expr, cty) -> Sexp_poly (s_expression expr, cty)
    | Pexp_object class_str -> Sexp_object class_str
    | Pexp_newtype (id, expr) -> Sexp_newtype (id, s_expression expr)
    | Pexp_pack mod_expr -> Sexp_pack (s_module_expr ~filename mod_expr)
    | Pexp_open (open_decl, expr) -> Sexp_open (open_decl, s_expression expr)
    | Pexp_letop letop -> Sexp_letop letop
    | Pexp_extension extension -> Sexp_extension extension
    | Pexp_unreachable -> Sexp_unreachable
  in
  mk_s_expression (spexp_desc expr.pexp_desc) loc loc_stack attributes

and s_module_expr ~filename { pmod_desc; pmod_loc; pmod_attributes } =
  let spmod_desc =
    match pmod_desc with
    | Pmod_ident id -> Smod_ident id
    | Pmod_structure str -> Smod_structure (structure ~filename str)
    | Pmod_functor (Unit, _) -> assert false (* TODO *)
    | Pmod_functor (Named (id, mod_type), mod_expr) ->
        Smod_functor
          ( id,
            Some (module_type ~filename mod_type),
            s_module_expr ~filename mod_expr )
    | Pmod_apply (mod_expr1, mod_expr2) ->
        Smod_apply
          (s_module_expr ~filename mod_expr1, s_module_expr ~filename mod_expr2)
    | Pmod_constraint (mod_expr, mod_type) ->
        Smod_constraint
          (s_module_expr ~filename mod_expr, module_type ~filename mod_type)
    | Pmod_unpack expr -> Smod_unpack (s_expression ~filename expr)
    | Pmod_extension extension -> Smod_extension extension
  in
  mk_s_module_expr spmod_desc pmod_loc pmod_attributes

and structure ~filename s =
  let add_str_item acc str_item = structure_item ~filename str_item :: acc in
  let structure = List.fold_left add_str_item [] s in
  List.rev (List.flatten structure)

and structure_item ~filename str_item =
  let loc = str_item.pstr_loc in
  match str_item.pstr_desc with
  | Pstr_eval (e, attrs) ->
      [ mk_s_structure_item (Str_eval (s_expression ~filename e, attrs)) ~loc ]
  | Pstr_value (rec_flag, vb_list) ->
      let vb_list = s_value_binding ~filename vb_list in
      let str_desc = mk_s_structure_item (Str_value (rec_flag, vb_list)) ~loc in
      [ str_desc ]
  | Pstr_type (rec_flag, type_decl_list) ->
      let td_list = List.map (type_declaration ~filename) type_decl_list in
      let str_desc = mk_s_structure_item (Str_type (rec_flag, td_list)) ~loc in
      [ str_desc ]
  | Pstr_attribute attr when is_spec attr ->
      [ mk_s_structure_item (floating_spec_str ~filename attr) ~loc ]
  | Pstr_attribute attr -> [ mk_s_structure_item (Str_attribute attr) ~loc ]
  | Pstr_module mod_binding ->
      [
        mk_s_structure_item
          (Str_module (s_module_binding ~filename mod_binding))
          ~loc;
      ]
  | Pstr_modtype mod_type_decl ->
      let s_mod_type = module_type_declaration ~filename mod_type_decl in
      [ mk_s_structure_item (Str_modtype s_mod_type) ~loc ]
  | Pstr_exception ty_exn -> [ mk_s_structure_item (Str_exception ty_exn) ~loc ]
  | Pstr_primitive _ -> assert false (* TODO *)
  | Pstr_typext _ -> assert false (* TODO *)
  | Pstr_recmodule _ -> assert false (* TODO *)
  | Pstr_open popen -> [ mk_s_structure_item (Str_open popen) ~loc ]
  | Pstr_class _ -> assert false (* TODO *)
  | Pstr_class_type _ -> assert false (* TODO *)
  | Pstr_include _ -> failwith "Include expressions are not supported yet"
  | Pstr_extension _ -> assert false
(* TODO *)

and s_value_binding ~filename vb_list =
  (* [val_binding v] parses the attributes of a value binding. As for val
     description, only the first attribute is considered as specification. *)
  let val_spec v =
    let spec, _ = get_spec_attr v.pvb_attributes in
    let val_spec =
      Option.map (parse_gospel ~filename Uparser.val_spec) spec
      |> Option.map snd
    in
    let expr = s_expression ~filename v.pvb_expr in
    mk_svb v.pvb_pat expr v.pvb_attributes val_spec v.pvb_loc
  in
  List.map val_spec vb_list

and s_module_binding ~filename { pmb_name; pmb_expr; pmb_attributes; pmb_loc } =
  {
    spmb_expr = s_module_expr ~filename pmb_expr;
    spmb_name = pmb_name;
    spmb_attributes = pmb_attributes;
    spmb_loc = pmb_loc;
  }
