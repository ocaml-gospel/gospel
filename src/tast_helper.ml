open Tast
open Symbols
module W = Warnings

let spec_arg label vs modified =
  {
    lb_vs = vs;
    lb_label = label;
    lb_consumes = None;
    lb_produces = None;
    lb_modified = modified;
  }

let ty_of_lb_arg arg = arg.lb_vs.vs_ty

let val_spec sp_args sp_ret sp_pre sp_checks sp_post sp_xpost sp_diverge sp_pure
    sp_equiv sp_text sp_loc =
  {
    sp_args;
    sp_ret;
    sp_pre;
    sp_checks;
    sp_post;
    sp_xpost;
    sp_diverge;
    sp_pure;
    sp_equiv;
    sp_text;
    sp_loc;
  }

(* Checks the following:
   1 - no duplicated args
   2 - pre and post of type prop

   TODO:
   1 - check what to do with writes
   2 - sp_xpost sp_reads sp_alias *)
let mk_val_spec args ret pre checks post xpost dv pure equiv txt loc =
  val_spec args ret pre checks post xpost dv pure equiv txt loc

let mk_val_description vd_name vd_type vd_prim vd_attrs vd_args vd_ret vd_spec
    vd_loc =
  { vd_name; vd_type; vd_prim; vd_attrs; vd_args; vd_ret; vd_spec; vd_loc }

let type_spec ty_ephemeral ty_model ty_invariants ty_text ty_loc =
  { ty_ephemeral; ty_model; ty_invariants; ty_text; ty_loc }

let label_declaration ld_field ld_mut ld_loc ld_attrs =
  { ld_field; ld_mut; ld_loc; ld_attrs }

let constructor_decl cd_cs cd_ld cd_loc cd_attrs =
  { cd_cs; cd_ld; cd_loc; cd_attrs }

let type_declaration td_ts td_params td_cstrs td_kind td_private td_manifest
    td_attrs td_spec td_loc =
  {
    td_ts;
    td_params;
    td_cstrs;
    td_kind;
    td_private;
    td_manifest;
    td_attrs;
    td_spec;
    td_loc;
  }

let axiom ax_name ax_term ax_loc ax_text = { ax_name; ax_term; ax_text; ax_loc }
let mk_axiom id t l = axiom id t l

let mk_fun_spec fun_req fun_ens fun_variant fun_coer fun_text fun_loc =
  { fun_req; fun_ens; fun_variant; fun_coer; fun_text; fun_loc }

let mk_function fun_ls fun_rec fun_params fun_def fun_spec fun_loc fun_text =
  { fun_ls; fun_rec; fun_params; fun_def; fun_spec; fun_loc; fun_text }

let extension_constructor id xs kd loc attrs =
  {
    ext_ident = id;
    ext_xs = xs;
    ext_kind = kd;
    ext_loc = loc;
    ext_attributes = attrs;
  }

let type_exception ctr loc attrs =
  { exn_constructor = ctr; exn_loc = loc; exn_attributes = attrs }

let sig_item sig_desc sig_loc = { sig_desc; sig_loc }
let mk_sig_item desc loc = sig_item desc loc
