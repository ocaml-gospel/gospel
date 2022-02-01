open Uast
open Tast
module Mstr = Map.Make (String)

type namespace = {
  ns_ts : Ttypes.tsymbol Mstr.t;
  (* XXX there we need a way to tell if the type is well constructed,
     which is not possible with the current Ttypes.tsymbol *)
  ns_sy : Symbols.symbol Mstr.t;
  ns_ns : namespace Mstr.t; (* XXX V1 has also a ns_tns, what is it? *)
}

type env = namespace
type err = BadArrity | WrongType (* XXX and ... *)

(* Should include a log for the warnings! (Maybe in the env type...) *)
(* Pros: we get rid of the result type! *)
(* This raise the question:
   in case of error, should we stop right away,
   or should we log an error, type with a default type and continue to have maximum errors? *)
type 'a t = (env * 'a, err) result
type ('a, 'b) typechecker = env -> 'a -> 'b t

let add _ _ _ = assert false

let args _env (ty : Ttypes.ty) args =
  if Ttypes.depth ty <> List.length args then assert false else assert false

let pattern env ty p = assert false
let term : (Uast.term, Tterm.term) typechecker = assert false
let xpost env x = assert false

let val_spec env ty spec =
  (* XXX TODO: handle environment passing *)
  let sp_args = args env ty spec.sp_header.sp_hd_args in
  let sp_ret = pattern env (Ttypes.last ty) spec.sp_header.sp_hd_ret in
  let sp_pre = List.map (term env) spec.sp_pre in
  let sp_checks = List.map (term env) spec.sp_checks in
  let sp_post = List.map (term env) spec.sp_post in
  let sp_xpost = List.map (xpost env) spec.sp_xpost in
  let sp_modifies = List.map (term env) spec.sp_modifies in
  let sp_consumes = List.map (term env) spec.sp_consumes in
  let sp_diverge = spec.sp_diverge in
  let sp_pure = spec.sp_pure in
  let sp_equiv = spec.sp_equiv in
  let sp_text = spec.sp_text in
  let loc = spec.sp_loc in
  ( env,
    {
      sp_args;
      sp_ret;
      sp_pre;
      sp_checks;
      sp_post;
      sp_xpost;
      sp_modifies;
      sp_consumes;
      sp_diverge;
      sp_pure;
      sp_equiv;
      sp_text;
      loc;
    } )

let sig_val env vd =
  let id = (* make symbol *) vd.vname in
  let ty = vd.vtype in
  let tvd : Tast.val_desc =
    (* build Tast.val_desc from Uast.s_val_description *) assert false
  in
  match vd.vspec with
  | None -> (add id ty env, tvd)
  | Some spec ->
      let env, spec = val_spec env ty spec in
      (add id ty env, { tvd with vd_spec = Some spec })

let sig_item env (s : Uast.s_signature_item_desc) =
  match s with
  | Uast.Sig_val vd -> sig_val env vd
  | Uast.Sig_type (_, _) -> assert false
  | Uast.Sig_typext _ -> assert false
  | Uast.Sig_module _ -> assert false
  | Uast.Sig_recmodule _ -> assert false
  | Uast.Sig_modtype _ -> assert false
  | Uast.Sig_exception _ -> assert false
  | Uast.Sig_open _ -> assert false
  | Uast.Sig_include _ -> assert false
  | Uast.Sig_class _ -> assert false
  | Uast.Sig_class_type _ -> assert false
  | Uast.Sig_attribute _ -> assert false
  | Uast.Sig_extension (_, _) -> assert false
  | Uast.Sig_function _ -> assert false
  | Uast.Sig_axiom _ -> assert false
  | Uast.Sig_ghost_type (_, _) -> assert false
  | Uast.Sig_ghost_val _ -> assert false
  | Uast.Sig_ghost_open _ -> assert false
