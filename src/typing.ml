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
open Utils
open Identifier
open Uast
open Ttypes
open Tmodule
open Tast_helper
open Symbols

(** Utils *)

type whereami =
  | Axiom
  | Checks
  | Consumes
  | Ensures
  | Function_or_predicate
  | Invariant
  | Modifies
  | Raises
  | Requires
  | Variant

let pid_of_label = function
  | Lunit -> invalid_arg "pid_of_label Lunit"
  | Lnone p | Loptional p | Lnamed p | Lghost (p, _) -> p

let string_list_of_qualid q =
  let rec fold_q acc = function
    | Qpreid pid -> pid.pid_str :: acc
    | Qdot (q, pid) -> fold_q (pid.pid_str :: acc) q
  in
  fold_q [] q

exception Ns_not_found of location * string

let rec q_loc = function Qpreid pid -> pid.pid_loc | Qdot (q, _) -> q_loc q

let ns_find ~loc f ns sl =
  match sl with
  | s :: _ :: _ when not (ns_exists_ns ns s) ->
      raise (Ns_not_found (loc, s))
      (* this will be caught to try to find file s *)
  | _ -> ( try f ns sl with Not_found -> W.error ~loc (W.Symbol_not_found sl))

let find_ts ~loc = ns_find ~loc ns_find_ts
let find_ls ~loc = ns_find ~loc ns_find_ls
let find_fd ~loc = ns_find ~loc ns_find_fd
let find_xs ~loc = ns_find ~loc ns_find_xs
let find_ns ~loc = ns_find ~loc ns_find_ns
let find_tns ~loc = ns_find ~loc ns_find_tns

let find_q (f : loc:Location.t -> 'a) ns q =
  let ln = string_list_of_qualid q in
  f ~loc:(q_loc q) ns ln

let find_q_ts = find_q find_ts
let find_q_ls = find_q find_ls
let find_q_fd = find_q find_fd
let find_q_xs = find_q find_xs
let find_q_ns = find_q find_ns

(** Typing types *)

(* specification types *)
let rec ty_of_pty ns = function
  | PTtyvar { pid_str; pid_loc; _ } ->
      { ty_node = Tyvar (tv_of_string ~loc:pid_loc pid_str) }
  | PTtyapp (q, ptyl) ->
      let ts = find_q_ts ns q in
      ty_app ts (List.map (ty_of_pty ns) ptyl)
  | PTtuple ptyl ->
      let tyl = List.map (ty_of_pty ns) ptyl in
      let ts = ts_tuple (List.length tyl) in
      ty_app ts tyl
  | PTarrow (_, pty1, pty2) ->
      let ty1, ty2 = (ty_of_pty ns pty1, ty_of_pty ns pty2) in
      ty_app ts_arrow [ ty1; ty2 ]

(* OCaml types *)
let rec ty_of_core ns cty =
  let loc = cty.ptyp_loc in
  match cty.ptyp_desc with
  | Ptyp_any -> { ty_node = Tyvar (create_tv (Ident.create ~loc "_")) }
  | Ptyp_var s -> { ty_node = Tyvar (tv_of_string ~loc s) }
  | Ptyp_tuple ctl ->
      let tyl = List.map (ty_of_core ns) ctl in
      ty_app ~loc (ts_tuple (List.length tyl)) tyl
  | Ptyp_constr (lid, ctl) ->
      let ts = find_ts ~loc:lid.loc ns (Longident.flatten_exn lid.txt) in
      let tyl = List.map (ty_of_core ns) ctl in
      ty_app ~loc ts tyl
  | Ptyp_arrow (_, ct1, ct2) ->
      (* TODO check what to do with the arg_label *)
      let ty1, ty2 = ((ty_of_core ns) ct1, (ty_of_core ns) ct2) in
      ty_app ~loc ts_arrow [ ty1; ty2 ]
  | _ -> assert false

(** Typing terms *)

open Dterm
open Tterm
open Tast

let dty_of_pty ns dty = dty_of_ty (ty_of_pty ns dty)

let find_constructors kid ts =
  match (Mid.find ts.ts_ident kid).sig_desc with
  | Sig_type (_, tdl, _) -> (
      match (List.find (fun td -> td.td_ts = ts) tdl).td_kind with
      | Pty_record { rd_cs; rd_ldl } ->
          (rd_cs, List.map (fun ld -> ld.ld_field) rd_ldl)
      | _ -> assert false)
  | _ -> assert false

let parse_record ~loc kid ns fll =
  let fll = List.map (fun (q, v) -> (find_q_fd ns q, v)) fll in
  let fs =
    match fll with
    | [] -> assert false (* foridden at parsing *)
    | (fs, _) :: _ -> fs
  in
  let ts =
    match fs.ls_args with
    | [ { ty_node = Tyapp (ts, _) } ] -> ts
    | _ -> W.error ~loc (W.Bad_record_field fs.ls_name.id_str)
  in
  let cs, pjl = find_constructors kid ts in
  let pjs = Sls.of_list pjl in
  let fll =
    List.fold_left
      (fun m (pj, v) ->
        if not (Sls.mem pj pjs) then
          W.error ~loc (W.Bad_record_field pj.ls_name.id_str)
        else if Mls.mem pj m then
          W.error ~loc (Duplicated_record_field pj.ls_name.id_str)
        else Mls.add pj v m)
      Mls.empty fll
  in
  (cs, pjl, fll)

let rec dpattern kid ns { pat_desc; pat_loc = loc } =
  let mk_dpattern ~loc dp_node dp_dty dp_vars =
    { dp_node; dp_dty; dp_vars; dp_loc = loc }
  in
  let mk_pwild loc dty = mk_dpattern ~loc DPwild dty Mstr.empty in
  let rec mk_papp ~loc cs dpl =
    let dtyl, dty = specialize_cs ~loc cs in
    match (dpl, cs.ls_args) with
    (* allow pattern C (x,y) when the constructor C expects only one
       argument, which can be a tuple (such as ('a * 'b) option) *)
    | _ :: _ :: _, [ _ ] ->
        let n = List.length dpl in
        let p = mk_papp ~loc (fs_tuple n) dpl in
        mk_papp ~loc cs [ p ]
    (* allow C _ with type t = C of int * int *)
    | [ { dp_node = DPwild; _ } ], _ :: _ :: _ ->
        let dpl = List.map (mk_pwild loc) dtyl in
        mk_papp ~loc cs dpl
    | _ ->
        app_unify ~loc cs dpattern_unify dpl dtyl;
        let check_duplicate s _ _ = W.error ~loc (W.Duplicated_variable s) in
        let vars =
          List.fold_left
            (fun acc dp -> Mstr.union check_duplicate acc dp.dp_vars)
            Mstr.empty dpl
        in
        mk_dpattern ~loc (DPapp (cs, dpl)) dty vars
  in
  match pat_desc with
  | Pwild ->
      let dty = dty_fresh () in
      mk_pwild loc dty
  | Pinterval (c1, c2) ->
      mk_dpattern ~loc (DPinterval (c1, c2)) dty_char Mstr.empty
  | Pvar pid ->
      let dty = dty_fresh () in
      let vars = Mstr.singleton pid.pid_str dty in
      mk_dpattern ~loc (DPvar pid) dty vars
  | Ptrue -> mk_papp ~loc fs_bool_true []
  | Pfalse -> mk_papp ~loc fs_bool_false []
  | Papp (q, pl) ->
      let cs = find_q_ls ns q in
      let dpl = List.map (dpattern kid ns) pl in
      mk_papp ~loc cs dpl
  | Ptuple pl ->
      let cs = fs_tuple (List.length pl) in
      let dpl = List.map (dpattern kid ns) pl in
      mk_papp ~loc cs dpl
  | Pas (p, pid) ->
      let dp = dpattern kid ns p in
      if Mstr.mem pid.pid_str dp.dp_vars then
        W.error ~loc:pid.pid_loc (W.Duplicated_variable pid.pid_str);
      let vars = Mstr.add pid.pid_str dp.dp_dty dp.dp_vars in
      mk_dpattern ~loc (DPas (dp, pid)) dp.dp_dty vars
  | Por (p1, p2) ->
      let dp1 = dpattern kid ns p1 in
      let dp2 = dpattern kid ns p2 in
      dpattern_unify dp1 dp2.dp_dty;
      let join v dty1 dty2 =
        match (dty1, dty2) with
        | Some dty1, Some dty2 ->
            dty_unify ~loc:dp1.dp_loc dty1 dty2;
            Some dty1
        | None, Some _ -> W.error ~loc:dp1.dp_loc (W.Unbound_variable v)
        | Some _, None -> W.error ~loc:dp2.dp_loc (W.Unbound_variable v)
        | None, None -> None
      in
      let vars = Mstr.merge join dp1.dp_vars dp2.dp_vars in
      mk_dpattern ~loc (DPor (dp1, dp2)) dp1.dp_dty vars
  | Pcast (p, pty) ->
      let dp = dpattern kid ns p in
      let dty = dty_of_pty ns pty in
      dpattern_unify dp dty;
      mk_dpattern ~loc (DPcast (dp, dty)) dty dp.dp_vars
  | Prec qpl ->
      let cs, pjl, fll = parse_record ~loc kid ns qpl in
      let get_pattern pj =
        try dpattern kid ns (Mls.find pj fll)
        with Not_found -> mk_pwild loc (dty_of_ty (Option.get pj.ls_value))
      in
      let dpl = List.map get_pattern pjl in
      mk_papp ~loc cs dpl
  | Pconst c ->
      let dty =
        match c with
        | Pconst_integer (_, None) -> dty_integer
        | Pconst_integer (s, (Some 'i' as c)) -> (
            try
              let (_ : int) = int_of_string s in
              dty_int
            with Failure _ -> W.error ~loc (W.Invalid_int_literal (s, c)))
        | Pconst_integer (_, _) ->
            (* This is forbidden at parsing*) assert false
        | Pconst_char _ -> dty_char
        | Pconst_string _ -> dty_string
        | Pconst_float _ -> dty_float
      in
      mk_dpattern ~loc (DPconst c) dty Mstr.empty

let binop = function
  | Uast.Tand -> Tand
  | Uast.Tand_asym -> Tand_asym
  | Uast.Tor -> Tor
  | Uast.Tor_asym -> Tor_asym
  | Uast.Timplies -> Timplies
  | Uast.Tiff -> Tiff

let rec dterm whereami kid crcm ns denv { term_desc; term_loc = loc } : dterm =
  let mk_dterm ~loc dt_node dty = { dt_node; dt_dty = dty; dt_loc = loc } in
  let apply dt1 t2 =
    let dt2 = dterm whereami kid crcm ns denv t2 in
    let dty = dty_fresh () in
    unify dt1 (Some (Tapp (ts_arrow, [ dty_of_dterm dt2; dty ])));
    let dt_app = DTapp (fs_apply, [ dt1; dt2 ]) in
    mk_dterm ~loc:dt2.dt_loc dt_app (Some dty)
  in
  (* CHECK location *)
  let map_apply dt tl = List.fold_left apply dt tl in
  let mk_app ~loc ls dtl =
    let dtyl, dty = specialize_ls ls in
    let dtl = app_unify_map ~loc ls (dterm_expected crcm) dtl dtyl in
    mk_dterm ~loc (DTapp (ls, dtl)) dty
  in
  let gen_app ~loc ls tl =
    let nls = List.length ls.ls_args and ntl = List.length tl in
    let args, extra = split_at_i nls tl in
    let dtl = List.map (dterm whereami kid crcm ns denv) args in
    let dtyl, dty = specialize_ls ls in
    if ntl < nls then
      let dtyl1, dtyl2 = split_at_i ntl dtyl in
      let dtl = List.map2 (dterm_expected crcm) dtl dtyl1 in
      let dty = Option.value ~default:dty_bool dty in
      let dty =
        List.fold_right
          (fun t1 t2 -> Dterm.Tapp (ts_arrow, [ t1; t2 ]))
          dtyl2 dty
      in
      mk_dterm ~loc (DTapp (ls, dtl)) (Some dty)
    else
      let dtl = List.map2 (dterm_expected crcm) dtl dtyl in
      let dt = mk_dterm ~loc (DTapp (ls, dtl)) dty in
      if extra = [] then dt else map_apply dt extra
  in
  let gen_app ~loc ls tl =
    (* gen_app in two layers, to check that constructors are fully
       applied (and with the usual syntax) without enforcing this on
       functions *)
    if ls.ls_constr then
      let n = List.length ls.ls_args in
      match tl with
      | [ { term_desc = Ttuple tl; _ } ] when List.length tl = n ->
          gen_app ~loc ls tl
      | [ { term_desc = Ttuple tl; _ } ] when n > 1 ->
          W.error ~loc (W.Bad_arity (ls.ls_name.id_str, n, List.length tl))
      | _ when List.length tl < n ->
          W.error ~loc (W.Partial_application ls.ls_name.id_str)
      | _ :: _ :: _ when not (is_fs_tuple ls || ls_equal ls fs_list_cons) ->
          W.error ~loc W.Syntax_error
      | _ -> gen_app ~loc ls tl
    else gen_app ~loc ls tl
  in
  let fun_app ~loc ls tl =
    if ls.ls_field then W.error ~loc (W.Field_application ls.ls_name.id_str);
    gen_app ~loc ls tl
  in
  let qualid_app q tl =
    match q with
    | Qpreid ({ pid_loc = loc; pid_str = s; _ } as pid) -> (
        match denv_get_opt denv s with
        | Some dty ->
            let dtv = mk_dterm ~loc (DTvar pid) (Some dty) in
            map_apply dtv tl
        | None -> fun_app ~loc (find_q_ls ns q) tl)
    | _ -> fun_app ~loc (find_q_ls ns q) tl
  in
  let rec unfold_app t1 t2 tl =
    match t1.term_desc with
    | Uast.Tpreid q -> qualid_app q (t2 :: tl)
    | Uast.Tapply (t11, t12) -> unfold_app t11 t12 (t2 :: tl)
    | _ ->
        let dt1 = dterm whereami kid crcm ns denv t1 in
        map_apply dt1 (t2 :: tl)
  in
  match term_desc with
  | Uast.Ttrue -> mk_dterm ~loc DTtrue (Some dty_bool)
  | Uast.Tfalse -> mk_dterm ~loc DTfalse (Some dty_bool)
  | Uast.Tconst c ->
      let dty =
        match c with
        | Pconst_integer (_, None) -> dty_integer
        | Pconst_integer (s, (Some 'i' as c)) -> (
            try
              let (_ : int) = int_of_string s in
              dty_int
            with Failure _ -> W.error ~loc (W.Invalid_int_literal (s, c)))
        | Pconst_integer (_, _) -> assert false
        | Pconst_char _ -> dty_char
        | Pconst_string _ -> dty_string
        | Pconst_float _ -> dty_float
      in
      mk_dterm ~loc (DTconst c) (Some dty)
  | Uast.Tpreid (Qpreid pid) when is_in_denv denv pid.pid_str ->
      let dty = denv_find ~loc:pid.pid_loc pid.pid_str denv in
      mk_dterm ~loc (DTvar pid) (Some dty)
  | Uast.Tpreid q ->
      (* in this case it must be a constant *)
      let ls = find_q_ls ns q in
      if ls.ls_field then
        W.error ~loc (W.Symbol_not_found (string_list_of_qualid q));
      gen_app ~loc ls []
  | Uast.Tfield (t, q) ->
      let ls = find_q_fd ns q in
      if not ls.ls_field then
        W.error ~loc (W.Bad_record_field ls.ls_name.id_str);
      gen_app ~loc ls [ t ]
  | Uast.Tidapp (q, tl) -> qualid_app q tl
  | Uast.Tapply (t1, t2) -> unfold_app t1 t2 []
  | Uast.Tnot t ->
      let dt = dterm whereami kid crcm ns denv t in
      dfmla_unify dt;
      mk_dterm ~loc (DTnot dt) dt.dt_dty
  | Uast.Tif (t1, t2, t3) ->
      let dt1 = dterm whereami kid crcm ns denv t1 in
      let dt2 = dterm whereami kid crcm ns denv t2 in
      let dt3 = dterm whereami kid crcm ns denv t3 in
      let dt1 = dfmla_expected crcm dt1 in
      let dty = max_dty crcm [ dt2; dt3 ] in
      let dt2 = dterm_expected_op crcm dt2 dty in
      let dt3 = dterm_expected_op crcm dt3 dty in
      mk_dterm ~loc (DTif (dt1, dt2, dt3)) dt2.dt_dty
  | Uast.Ttuple [] -> fun_app ~loc fs_unit []
  | Uast.Ttuple tl -> fun_app ~loc (fs_tuple (List.length tl)) tl
  | Uast.Tlet (pid, t1, t2) ->
      let dt1 = dterm whereami kid crcm ns denv t1 in
      let denv = denv_add_var denv pid.pid_str (dty_of_dterm dt1) in
      let dt2 = dterm whereami kid crcm ns denv t2 in
      mk_dterm ~loc (DTlet (pid, dt1, dt2)) dt2.dt_dty
  | Uast.Tinfix (t1, op1, t23) ->
      let apply de1 op de2 =
        let symbol =
          if op.Preid.pid_str = neq.id_str then eq.id_str else op.pid_str
        in
        let ls = find_ls ~loc:op1.pid_loc ns [ symbol ] in
        let dtyl, dty = specialize_ls ls in
        (if ls_equal ls ps_equ then
           let max = max_dty crcm [ de1; de2 ] in
           try
             dty_unify ~loc (Option.value max ~default:dty_bool) (List.hd dtyl)
           with Exit -> ());
        let dtl =
          app_unify_map ~loc ls (dterm_expected crcm) [ de1; de2 ] dtyl
        in
        if op.pid_str = neq.id_str then
          mk_dterm ~loc (DTnot (mk_dterm ~loc (DTapp (ls, dtl)) dty)) None
        else mk_dterm ~loc (DTapp (ls, dtl)) dty
      in
      let rec chain _ de1 op1 t23 =
        match t23 with
        | { term_desc = Uast.Tinfix (t2, op2, t3); term_loc = loc23 } ->
            let de2 = dterm whereami kid crcm ns denv t2 in
            (* TODO: improve locations of subterms. See loc_cutoff function in why3 typing.ml *)
            (* let loc12 = loc_cutoff loc loc23 t2.term_loc in *)
            let de12 = apply de1 op1 de2 in
            let de23 = chain loc23 de2 op2 t3 in
            dfmla_unify de12;
            dfmla_unify de23;
            mk_dterm ~loc (DTbinop (Tand, de12, de23)) None
        | _ -> apply de1 op1 (dterm whereami kid crcm ns denv t23)
      in
      chain loc (dterm whereami kid crcm ns denv t1) op1 t23
  | Uast.Tbinop (t1, op, t2) ->
      let dt1 = dterm whereami kid crcm ns denv t1 in
      let dt2 = dterm whereami kid crcm ns denv t2 in
      dfmla_unify dt1;
      dfmla_unify dt2;
      mk_dterm ~loc (DTbinop (binop op, dt1, dt2)) None
  | Uast.Tquant (q, vl, t) ->
      let get_dty pty =
        match pty with None -> dty_fresh () | Some pty -> dty_of_pty ns pty
      in
      let vl = List.map (fun (pid, pty) -> (pid, get_dty pty)) vl in
      let denv = denv_add_var_quant denv vl in
      let dt = dterm whereami kid crcm ns denv t in
      let dty, q =
        match q with
        | Uast.Tforall ->
            dfmla_unify dt;
            (None, Tforall)
        | Uast.Texists ->
            dfmla_unify dt;
            (None, Texists)
      in
      mk_dterm ~loc (DTquant (q, vl, dt)) dty
  | Uast.Tlambda (pl, t, pty) ->
      let arg p =
        let dty = dty_fresh () and dp = dpattern kid ns p in
        dpattern_unify dp dty;
        (dp, dty)
      in
      let args = List.map arg pl in
      let choose_snd _ _ vs = Some vs in
      let denv =
        List.fold_left
          (fun denv (dp, _) -> Mstr.union choose_snd denv dp.dp_vars)
          denv args
      in
      let dt = dterm whereami kid crcm ns denv t in
      let dt =
        match pty with
        | Some pty -> dterm_expected crcm dt (dty_of_pty ns pty)
        | _ -> dt
      in
      let dt_dty = dty_of_dterm dt in
      let dty =
        let apply (_, dty1) dty2 = Dterm.Tapp (ts_arrow, [ dty1; dty2 ]) in
        Some (List.fold_right apply args dt_dty)
      in
      mk_dterm ~loc (DTlambda (List.map fst args, dt)) dty
  | Uast.Tcase (t, ptl) ->
      let dt = dterm whereami kid crcm ns denv t in
      let dt_dty = dty_of_dterm dt in
      let branch (p, g, t) =
        let dp = dpattern kid ns p in
        dpattern_unify dp dt_dty;
        let choose_snd _ _ x = Some x in
        let denv = Mstr.union choose_snd denv dp.dp_vars in
        let dt = dterm whereami kid crcm ns denv t in
        let dg =
          match g with
          | None -> None
          | Some g -> Some (dterm whereami kid crcm ns denv g)
        in
        (dp, dg, dt)
      in
      let pdtl = List.map branch ptl in
      let dty = max_dty crcm (List.map (fun (_p, _g, t) -> t) pdtl) in
      let pdtl =
        List.map
          (fun (pat, guard, dt) ->
            ( pat,
              (match guard with
              | None -> None
              | Some g -> Some (dfmla_expected crcm g)),
              dterm_expected_op crcm dt dty ))
          pdtl
      in
      mk_dterm ~loc (DTcase (dt, pdtl)) dty
  | Uast.Tcast (t, pty) ->
      let dt = dterm whereami kid crcm ns denv t in
      let dty = dty_of_pty ns pty in
      dterm_expected crcm dt dty
  | Uast.Tscope (q, t) ->
      let ns = find_q_ns ns q in
      dterm whereami kid crcm ns denv t
  | Uast.Tattr (at, t) ->
      let dt = dterm whereami kid crcm ns denv t in
      mk_dterm ~loc (DTattr (dt, [ at ])) dt.dt_dty
  | Uast.Told t -> (
      match whereami with
      | Requires -> W.(error ~loc (Old_in_precond "requires"))
      | Checks -> W.(error ~loc (Old_in_precond "checks"))
      | _ ->
          let dt = dterm whereami kid crcm ns denv t in
          mk_dterm ~loc (DTold dt) dt.dt_dty)
  | Uast.Trecord qtl ->
      let cs, pjl, fll = parse_record ~loc kid ns qtl in
      let get_term pj =
        try dterm whereami kid crcm ns denv (Mls.find pj fll)
        with Not_found ->
          W.error ~loc (W.Unknown_record_field pj.ls_name.id_str)
      in
      mk_app ~loc cs (List.map get_term pjl)
  | Uast.Tupdate (t, qtl) ->
      let cs, pjl, fll = parse_record ~loc kid ns qtl in
      let get_term pj =
        try dterm whereami kid crcm ns denv (Mls.find pj fll)
        with Not_found -> fun_app ~loc:t.term_loc pj [ t ]
      in
      mk_app ~loc:t.term_loc cs (List.map get_term pjl)

let dterm whereami kid crcm ns env t =
  let denv = Mstr.map (fun vs -> dty_of_ty vs.vs_ty) env in
  dterm whereami kid crcm ns denv t

let term_with_unify whereami kid crcm ty ns env t =
  let dt = dterm whereami kid crcm ns env t in
  dterm_unify dt (dty_of_ty ty);
  term env dt

let fmla whereami kid crcm ns env t =
  let dt = dterm whereami kid crcm ns env t in
  let tt = fmla env dt in
  { tt with t_loc = t.term_loc }

let private_flag = function
  | Asttypes.Private -> Private
  | Asttypes.Public -> Public

let rec_flag = function
  | Asttypes.Nonrecursive -> Nonrecursive
  | Asttypes.Recursive -> Recursive

let mutable_flag = function
  | Asttypes.Mutable -> Mutable
  | Asttypes.Immutable -> Immutable

let process_type_spec kid crcm ns ty spec =
  let field (ns, fields) f =
    let f_ty = ty_of_pty ns f.f_pty in
    let ls = fsymbol ~field:true (Ident.of_preid f.f_preid) [ ty ] f_ty in
    ( ns_add_fd ~allow_duplicate:true ns f.f_preid.pid_str ls,
      (ls, f.f_mutable) :: fields )
  in
  let ns, fields = List.fold_left field (ns, []) spec.ty_field in
  let fields = List.rev fields in
  let aux = function
    | vs, xs ->
        let self_vs = create_vsymbol vs ty in
        let env = Mstr.singleton self_vs.vs_name.id_str self_vs in
        (self_vs, List.map (fmla Invariant kid crcm ns env) xs)
  in
  let invariants = Option.map aux spec.ty_invariant in
  type_spec spec.ty_ephemeral fields invariants spec.ty_text spec.ty_loc

(* TODO compare manifest with td_kind *)
let type_type_declaration kid crcm ns r tdl =
  let add_new tdm td =
    if Mstr.mem td.tname.txt tdm then
      W.error ~loc:td.tname.loc (W.Name_clash td.tname.txt)
    else Mstr.add td.tname.txt td tdm
  in
  let tdm = List.fold_left add_new Mstr.empty tdl in
  let hts = Hashtbl.create 0 in
  let htd = Hashtbl.create 0 in
  let rec parse_core alias tvl core =
    let loc = core.ptyp_loc in
    match core.ptyp_desc with
    | Ptyp_any -> W.error ~loc (W.Unsupported "_ type parameters")
    | Ptyp_var s -> (
        try { ty_node = Tyvar (Mstr.find s tvl) }
        with Not_found -> W.error ~loc:core.ptyp_loc (W.Unbound_variable s))
    | Ptyp_arrow (_lbl, ct1, ct2) ->
        let ty1, ty2 = (parse_core alias tvl ct1, parse_core alias tvl ct2) in
        ty_app ts_arrow [ ty1; ty2 ]
    | Ptyp_tuple ctl ->
        let tyl = List.map (parse_core alias tvl) ctl in
        ty_app (ts_tuple (List.length tyl)) tyl
    | Ptyp_constr (lid, ctl) ->
        let idl = Longident.flatten_exn lid.txt in
        let tyl = List.map (parse_core alias tvl) ctl in
        let ts =
          match idl with
          | [ s ] when r = Recursive && Sstr.mem s alias ->
              W.error ~loc (W.Cyclic_type_declaration s)
          | [ s ] when Hashtbl.mem hts s -> Hashtbl.find hts s
          | [ s ] when r = Recursive && Mstr.mem s tdm ->
              visit ~alias:(Sstr.add s alias) s (Mstr.find s tdm);
              Hashtbl.find hts s
          | s -> find_ts ~loc:lid.loc ns s
        in
        if List.length tyl <> ts_arity ts then
          W.error ~loc
            (W.Bad_type_arity (ts.ts_ident.id_str, ts_arity ts, List.length tyl));
        ty_app ts tyl
    | _ -> assert false
  (* TODO what to do with other cases? *)
  and visit ~alias s td =
    let parse_params (ct, vi) (tvl, params, vs) =
      let loc = ct.ptyp_loc in
      match ct.ptyp_desc with
      | Ptyp_var s ->
          let tv = tv_of_string ~loc s in
          (Mstr.add s tv tvl, tv :: params, vi :: vs)
      | Ptyp_any -> W.error ~loc (W.Unsupported "_ type parameters")
      | _ -> assert false
      (* should not happen -- see parser optional_type_variable *)
    in

    let tvl, params, variance_list =
      List.fold_right parse_params td.tparams (Mstr.empty, [], [])
    in

    let manifest =
      let alias =
        match r with Nonrecursive -> alias | _ -> Sstr.add s alias
      in
      Option.map (parse_core alias tvl) td.tmanifest
    in
    let td_ts = mk_ts (Ident.create ~loc:td.tname.loc s) params manifest in
    Hashtbl.add hts s td_ts;

    let process_record ty alias ldl =
      let cs_id = Ident.create ~loc:Location.none ("constr#" ^ s) in
      let fields_ty =
        List.map (fun ld -> parse_core alias tvl ld.pld_type) ldl
      in
      let rd_cs = fsymbol ~constr:true ~field:false cs_id fields_ty ty in
      let mk_ld ld (ldl, ns) =
        let id = Ident.create ~loc:ld.pld_loc ld.pld_name.txt in
        let ty_res = parse_core alias tvl ld.pld_type in
        let field = fsymbol ~field:true id [ ty ] ty_res in
        let mut = mutable_flag ld.pld_mutable in
        let ld = label_declaration field mut ld.pld_loc ld.pld_attributes in
        (ld :: ldl, ns_add_fd ~allow_duplicate:true ns id.id_str field)
      in
      let rd_ldl, ns = List.fold_right mk_ld ldl ([], ns) in
      ({ rd_cs; rd_ldl }, ns)
    in

    let process_variant ty_res alias cd (acc, ns) =
      let loc = cd.pcd_loc in
      if cd.pcd_res != None then
        W.error ~loc (W.Unsupported "type in constructors");
      let cs_id = Ident.create ~loc cd.pcd_name.txt in
      let cd_cs, cd_ld, ns =
        match cd.pcd_args with
        | Pcstr_tuple ctl ->
            let tyl = List.map (parse_core alias tvl) ctl in
            let ls = fsymbol ~constr:true ~field:false cs_id tyl ty_res in
            (ls, [], ns_add_ls ~allow_duplicate:true ns cs_id.id_str ls)
        | Pcstr_record ldl ->
            let add ld (ldl, tyl, ns) =
              let id = Ident.create ~loc:ld.pld_loc ld.pld_name.txt in
              let ty = parse_core alias tvl ld.pld_type in
              let mut = mutable_flag ld.pld_mutable in
              let field = fsymbol ~constr:false ~field:true id [ ty_res ] ty in
              let ld =
                label_declaration (id, ty) mut ld.pld_loc ld.pld_attributes
              in
              ( ld :: ldl,
                ty :: tyl,
                ns_add_fd ~allow_duplicate:true ns id.id_str field )
            in
            let ldl, tyl, ns = List.fold_right add ldl ([], [], ns) in
            let cs = fsymbol ~constr:true ~field:false cs_id tyl ty_res in
            let ns = ns_add_ls ~allow_duplicate:true ns cs_id.id_str cs in
            (cs, ldl, ns)
      in
      (constructor_decl cd_cs cd_ld cd.pcd_loc cd.pcd_attributes :: acc, ns)
    in

    let ty = ty_app td_ts (List.map ty_of_var params) in
    let kind, ns =
      let alias = Sstr.empty in
      match td.tkind with
      | Ptype_abstract -> (Pty_abstract, ns)
      | Ptype_variant cdl
        when List.exists (fun cd -> Option.is_some cd.pcd_res) cdl ->
          (* GADT *)
          (* TODO: Add a warning here *)
          (Pty_abstract, ns)
      | Ptype_variant cdl ->
          let v, ns = List.fold_right (process_variant ty alias) cdl ([], ns) in
          (Pty_variant v, ns)
      | Ptype_record ldl ->
          let record, ns = process_record ty alias ldl in
          (Pty_record record, ns)
      | Ptype_open -> assert false
    in

    (* invariants are only allowed on abstract/private types *)
    (match ((td.tkind, td.tmanifest), td.tspec) with
    | ( ((Ptype_variant _ | Ptype_record _), _ | _, Some _),
        Some { ty_invariant = Some _; _ } )
      when td.tprivate = Public ->
        W.error ~loc:td.tloc (W.Public_type_invariant td_ts.ts_ident.id_str)
    | _, _ -> ());

    let params = List.combine params variance_list in

    (* We add a temporary type declaration without spec, to be able to type
       recursive invariants and check exhaustivity for pattern-matchings. We
       replace it with the real one afterwards. *)
    let inv_td =
      type_declaration td_ts params [] kind (private_flag td.tprivate) manifest
        td.tattributes None td.tloc
    in
    Hts.add type_declarations td_ts inv_td;

    let spec = Option.map (process_type_spec kid crcm ns ty) td.tspec in

    if td.tcstrs != [] then
      W.error ~loc:td.tloc (W.Unsupported "type constraints");

    let td =
      type_declaration td_ts params [] kind (private_flag td.tprivate) manifest
        td.tattributes spec td.tloc
    in
    Hashtbl.add htd s td
  in

  Mstr.iter (visit ~alias:Sstr.empty) tdm;
  let tdl = List.map (fun td -> Hashtbl.find htd td.tname.txt) tdl in
  List.iter (fun td -> Hts.replace type_declarations td.td_ts td) tdl;
  tdl

let process_sig_type ~loc ?(ghost = Nonghost) kid crcm ns r tdl =
  let r = rec_flag r in
  let tdl = type_type_declaration kid crcm ns r tdl in
  let sig_desc = Sig_type (r, tdl, ghost) in
  mk_sig_item sig_desc loc

(** Type val declarations *)

let rec val_parse_core_type ns cty =
  match cty.ptyp_desc with
  | Ptyp_arrow (lbl, ct1, ct2) ->
      let args, res = val_parse_core_type ns ct2 in
      ((ty_of_core ns ct1, lbl) :: args, res)
  | _ -> ([], ty_of_core ns cty)

(* Checks the following
   1 - the val id string is equal to the name in val header
   2 - no duplicated names in arguments and arguments in header
   match core type
*)
let process_val_spec kid crcm ns id args ret vs =
  let header = Option.get vs.sp_header in
  let loc = header.sp_hd_nm.pid_loc in
  let id_val = id.Ident.id_str in
  let id_spec = header.sp_hd_nm.pid_str in
  let cmp_ids =
    match String.split_on_char ' ' id_spec with
    | [ "infix"; s ] -> String.equal s id_val
    | [ _ ] -> id_val = id_spec
    | _ -> assert false
  in
  if not cmp_ids then
    W.type_checking_error ~loc "val specification header does not match name";

  let add_arg la env lal =
    match la with
    | Lunit -> (env, la :: lal)
    | _ ->
        let vs = vs_of_lb_arg la in
        let vs_str = vs.vs_name.id_str in
        let add = function
          | None -> Some vs
          | Some _ ->
              W.error ~loc:vs.vs_name.id_loc (W.Duplicated_variable vs_str)
        in
        (Mstr.update vs_str add env, la :: lal)
  in

  let process_args where args tyl env lal =
    let wh_msg =
      match where with `Parameter -> "parameter" | `Return -> "returned value"
    in
    let mismatch_msg = wh_msg ^ " does not match with val type" in
    let global_tyl = tyl in
    let rec aux args tyl env lal =
      match (args, tyl) with
      | [], [] -> (env, List.rev lal)
      | [], _ ->
          let msg =
            match (where, global_tyl) with
            | `Return, _ :: _ :: _ ->
                "too few returned values: when a function returns a tuple, the \
                 gospel header should name each member of the tuple; so the \
                 header of a function returning a pair might be \"x,y = ...\""
            | `Return, _ -> "too few returned values"
            | `Parameter, _ -> "too few parameters"
          in
          W.type_checking_error ~loc:header.sp_hd_nm.pid_loc msg
      | Uast.Lghost (pid, pty) :: args, _ ->
          let ty = ty_of_pty ns pty in
          let vs = create_vsymbol pid ty in
          let env, lal = add_arg (Lghost vs) env lal in
          aux args tyl env lal
      | Loptional pid :: args, (ty, Asttypes.Optional s) :: tyl ->
          if not (String.equal pid.pid_str s) then
            W.type_checking_error ~loc:pid.pid_loc mismatch_msg;
          let ty = ty_app ts_option [ ty ] in
          let vs = create_vsymbol pid ty in
          let env, lal = add_arg (Loptional vs) env lal in
          aux args tyl env lal
      | Lnamed pid :: args, (ty, Asttypes.Labelled s) :: tyl ->
          if not (String.equal pid.pid_str s) then
            W.type_checking_error ~loc:pid.pid_loc mismatch_msg;
          let vs = create_vsymbol pid ty in
          let env, lal = add_arg (Lnamed vs) env lal in
          aux args tyl env lal
      | Lnone pid :: args, (ty, Asttypes.Nolabel) :: tyl ->
          let vs = create_vsymbol pid ty in
          let env, lal = add_arg (Lnone vs) env lal in
          aux args tyl env lal
      | Lunit :: args, _ :: tyl -> aux args tyl env (Lunit :: lal)
      | _, [] ->
          let msg = "too many " ^ wh_msg ^ "s" in
          W.type_checking_error ~loc:header.sp_hd_nm.pid_loc msg
      | la :: _, _ ->
          W.type_checking_error ~loc:(pid_of_label la).pid_loc mismatch_msg
    in
    aux args tyl env lal
  in

  let env, args =
    process_args `Parameter header.sp_hd_args args Mstr.empty []
  in

  let pre = List.map (fmla Requires kid crcm ns env) vs.sp_pre in
  let checks = List.map (fmla Checks kid crcm ns env) vs.sp_checks in
  let wr =
    List.map
      (fun t -> dterm Modifies kid crcm ns env t |> term env)
      vs.sp_writes
  in
  let cs =
    List.map
      (fun t -> dterm Consumes kid crcm ns env t |> term env)
      vs.sp_consumes
  in

  let process_xpost (loc, exn) =
    let merge_xpost t tl =
      match (t, tl) with
      | None, None -> Some []
      | None, Some tl -> Some tl
      | Some t, None -> Some [ t ]
      | Some t, Some tl -> Some (t :: tl)
    in
    let process mxs (q, pt) =
      let xs = find_q_xs ns q in
      match pt with
      | None -> (
          match xs.xs_type with
          | Exn_tuple [] -> Mxs.update xs (merge_xpost None) mxs
          | _ ->
              W.type_checking_error ~loc
                "Exception pattern does not match its type")
      | Some (p, t) ->
          let dp = dpattern kid ns p in
          let ty =
            let rec aux p xs =
              match (p.pat_desc, xs.xs_type) with
              | (Pvar _ | Pwild), Exn_tuple [] ->
                  W.type_checking_error ~loc "Exception pattern not expected"
              | ( ( Pvar _ | Pwild | Ptuple _ | Pconst _
                  | Pinterval (_, _)
                  | Ptrue | Pfalse ),
                  Exn_tuple [ ty ] ) ->
                  ty
              | (Pvar _ | Pwild), Exn_tuple (_ :: _ :: _) ->
                  W.type_checking_error ~loc
                    "Exception pattern doesn't match its type"
              | Ptuple _, Exn_tuple tyl ->
                  ty_app (ts_tuple (List.length tyl)) tyl
              | Pas (p, _), _ -> aux p xs
              | Por (p0, p1), _ ->
                  let ty0 = aux p0 xs and ty1 = aux p1 xs in
                  if Ttypes.ty_equal ty0 ty1 then ty0
                  else W.type_checking_error ~loc "Type mismatch"
              | Prec _, Exn_record _ ->
                  (* TODO unify types and field names *)
                  W.error ~loc (W.Unsupported "Record type in exceptions")
              | Pcast (_, _), _ ->
                  (* This is not handled in Dterm.pattern *)
                  W.error ~loc (W.Unsupported "Cast in exceptions")
              | Papp (_, _), _ ->
                  W.error ~loc (W.Unsupported "Qualified pattern in exceptions")
              | _, _ ->
                  W.type_checking_error ~loc
                    "Exception pattern does not match its type"
            in
            aux p xs
          in
          dpattern_unify dp (dty_of_ty ty);
          let p, vars = pattern dp in
          let choose_snd _ _ vs = Some vs in
          let env = Mstr.union choose_snd env vars in
          let t = fmla Raises kid crcm ns env t in
          Mxs.update xs (merge_xpost (Some (p, t))) mxs
    in
    List.fold_left process Mxs.empty exn |> Mxs.bindings
  in
  let xpost =
    List.fold_right (fun xp acc -> process_xpost xp @ acc) vs.sp_xpost []
  in

  let env, ret =
    match (header.sp_hd_ret, ret.ty_node) with
    | [], _ -> (env, [])
    | _, Tyapp (ts, tyl) when is_ts_tuple ts ->
        let tyl = List.map (fun ty -> (ty, Asttypes.Nolabel)) tyl in
        process_args `Return header.sp_hd_ret tyl env []
    | _, _ ->
        process_args `Return header.sp_hd_ret [ (ret, Asttypes.Nolabel) ] env []
  in
  let post = List.map (fmla Ensures kid crcm ns env) vs.sp_post in

  if vs.sp_pure then (
    if vs.sp_diverge then
      W.type_checking_error ~loc "a pure function cannot diverge";
    if wr <> [] then
      W.type_checking_error ~loc "a pure function cannot have writes";
    if xpost <> [] || checks <> [] then
      W.type_checking_error ~loc "a pure function cannot raise exceptions");
  mk_val_spec args ret pre checks post xpost wr cs vs.sp_diverge vs.sp_pure
    vs.sp_equiv vs.sp_text vs.sp_loc

let empty_spec preid ret args =
  {
    sp_header = Some { sp_hd_nm = preid; sp_hd_ret = ret; sp_hd_args = args };
    sp_pre = [];
    sp_checks = [];
    sp_post = [];
    sp_xpost = [];
    sp_writes = [];
    sp_consumes = [];
    sp_diverge = false;
    sp_pure = false;
    sp_equiv = [];
    sp_text = "";
    sp_loc = Location.none;
  }

let mk_dummy_var i (ty, arg) =
  let loc = Location.none in
  match arg with
  | _ when ty_equal ty ty_unit -> Uast.Lunit
  | Nolabel -> Uast.Lnone (Preid.create ~loc ("__arg" ^ string_of_int i))
  | Labelled s -> Uast.Lnamed (Preid.create ~loc s)
  | Optional s -> Uast.Loptional (Preid.create ~loc s)

let process_val ~loc ?(ghost = Nonghost) kid crcm ns vd =
  let id = Ident.create ~loc:vd.vloc vd.vname.txt in
  let args, ret = val_parse_core_type ns vd.vtype in
  let spec =
    match vd.vspec with
    | None | Some { sp_header = None; _ } -> (
        let id = Preid.create ~loc:vd.vloc vd.vname.txt in
        let rets =
          match ret with
          | { ty_node = Tyapp (ts, _) } when is_ts_tuple ts ->
              let arity = ts_arity ts in
              List.init arity (fun i ->
                  Uast.Lnone (Preid.create ~loc:vd.vloc (Fmt.str "result%d" i)))
          | _ ->
              [
                Uast.Lnone
                  (if args = [] then id else Preid.create ~loc:vd.vloc "result");
              ]
        in
        let args = List.mapi mk_dummy_var args in
        match vd.vspec with
        | None -> empty_spec id rets args
        | Some s ->
            {
              s with
              sp_header =
                Some { sp_hd_nm = id; sp_hd_ret = rets; sp_hd_args = args };
            })
    | Some s -> s
  in
  let spec = process_val_spec kid crcm ns id args ret spec in
  let so = Option.map (fun _ -> spec) vd.vspec in
  let () =
    (* check there is a modifies clause if the return type is unit, throw a warning if not *)
    if Ttypes.(ty_equal ret ty_unit) && args <> [] then
      match so with
      | None -> ()
      | Some sp ->
          if sp.sp_wr = [] then
            W.error ~loc (W.Return_unit_without_modifies id.id_str)
  in
  let vd =
    mk_val_description id vd.vtype vd.vprim vd.vattributes spec.sp_args
      spec.sp_ret so vd.vloc
  in
  mk_sig_item (Sig_val (vd, ghost)) loc

(** Typing function, axiom, and exception declarations *)

(* Currently checking:
   1 - arguments have different names *)
let process_function kid crcm ns f =
  let f_ty = Option.map (ty_of_pty ns) f.fun_type in

  let params =
    List.map
      (fun (_, pid, pty) -> create_vsymbol pid (ty_of_pty ns pty))
      f.fun_params
  in
  let tyl = List.map (fun vs -> vs.vs_ty) params in

  let ls = lsymbol ~field:false (Ident.of_preid f.fun_name) tyl f_ty in
  let ns =
    if f.fun_rec then ns_add_ls ~allow_duplicate:true ns f.fun_name.pid_str ls
    else ns
  in

  (* check that there is no duplicated parameters; we must do this
     here, before creating identifiers *)
  let add_var nm vs = function
    | None -> Some vs
    | Some _ -> W.error ~loc:vs.vs_name.id_loc (W.Duplicated_variable nm)
  in
  let env =
    List.fold_left
      (fun env vs ->
        let nm = vs.vs_name.id_str in
        Mstr.update nm (add_var nm vs) env)
      Mstr.empty params
  in
  let env, result =
    match f_ty with
    | None -> (env, None)
    | Some ty ->
        let result = create_vsymbol (Preid.create ~loc:f.fun_loc "result") ty in
        (Mstr.add "result" result env, Some result)
  in

  let def =
    match f_ty with
    | None -> Option.map (fmla Function_or_predicate kid crcm ns env) f.fun_def
    | Some ty ->
        Option.map
          (term_with_unify Function_or_predicate kid crcm ty ns env)
          f.fun_def
  in

  let spec =
    Option.map
      (fun (spec : Uast.fun_spec) ->
        let req = List.map (fmla Requires kid crcm ns env) spec.fun_req in
        let ens = List.map (fmla Ensures kid crcm ns env) spec.fun_ens in
        let variant =
          List.map
            (term_with_unify Variant kid crcm ty_integer ns env)
            spec.fun_variant
        in
        mk_fun_spec req ens variant spec.fun_coer spec.fun_text spec.fun_loc)
      f.fun_spec
  in
  let f =
    mk_function ?result ls f.fun_rec params def spec f.fun_loc f.fun_text
  in
  mk_sig_item (Sig_function f) f.fun_loc

let process_axiom loc kid crcm ns a =
  let id = Ident.of_preid a.Uast.ax_name in
  let t = fmla Axiom kid crcm ns Mstr.empty a.Uast.ax_term in
  let ax = mk_axiom id t a.ax_loc a.ax_text in
  mk_sig_item (Sig_axiom ax) loc

let process_exception_sig loc ns te =
  let ec = te.ptyexn_constructor in
  let id = Ident.create ~loc:ec.pext_loc ec.pext_name.txt in
  let xs =
    match ec.pext_kind with
    | Pext_rebind lid -> find_xs ~loc:lid.loc ns (Longident.flatten_exn lid.txt)
    | Pext_decl ([], ca, None) ->
        let args =
          match ca with
          | Pcstr_tuple ctyl -> Exn_tuple (List.map (ty_of_core ns) ctyl)
          | Pcstr_record ldl ->
              let get { pld_name; pld_type; _ } =
                ( Ident.create ~loc:pld_name.loc pld_name.txt,
                  ty_of_core ns pld_type )
              in
              Exn_record (List.map get ldl)
        in
        xsymbol id args
    | Pext_decl (_, _, _) ->
        W.error ~loc (W.Unsupported "type of exception declaration")
  in
  let ec =
    extension_constructor id xs ec.pext_kind ec.pext_loc ec.pext_attributes
  in
  let te = type_exception ec te.ptyexn_loc te.ptyexn_attributes in
  mk_sig_item (Sig_exception te) loc

(** Typing use, and modules *)

type parse_env = {
  lpaths : string list;
  (* loading paths *)
  parsing : Utils.Sstr.t;
      (* files being parsed; used to avoid circular dependencies *)
}

let penv lpaths parsing =
  { lpaths = lpaths @ [ Findlib.package_directory "stdlib" ]; parsing }

let rec open_file ~loc penv muc nm =
  if Sstr.mem nm penv.parsing then W.error ~loc W.Circular_open;
  try add_ns ~export:true muc nm (get_file muc nm).fl_export
  with Not_found ->
    let file_nm = String.uncapitalize_ascii nm ^ ".mli" in
    let sl =
      let file = Parser_frontend.with_loadpath penv.lpaths file_nm in
      Parser_frontend.parse_ocaml_gospel file
    in
    let muc = open_empty_module muc nm in
    let penv = { penv with parsing = Sstr.add nm penv.parsing } in
    let muc = List.fold_left (type_sig_item penv) muc sl in
    let muc = close_module_file muc in
    muc

and module_as_file ~loc penv muc nm =
  try open_file ~loc penv muc nm
  with Not_found -> W.error ~loc (W.Module_not_found nm)

and process_open ~loc ?(ghost = Nonghost) penv muc od =
  let qd = Longident.flatten_exn od.popen_expr.txt in
  let qd_loc = od.popen_loc in
  let hd = List.hd qd in
  let muc =
    if ns_exists_ns (get_top_import muc) hd then muc
    else module_as_file ~loc:qd_loc penv muc hd
  in
  let od =
    {
      opn_id = qd;
      opn_override = od.popen_override;
      opn_loc = od.popen_loc;
      opn_attrs = od.popen_attributes;
    }
  in
  (muc, mk_sig_item (Sig_open (od, ghost)) loc)

(* assumes that a new namespace has been opened *)
and process_modtype penv muc umty =
  match umty.mdesc with
  | Mod_signature usig ->
      let muc = List.fold_left (type_sig_item penv) muc usig in
      let tsig = Mod_signature (get_top_sigs muc) in
      let tmty =
        { mt_desc = tsig; mt_loc = umty.mloc; mt_attrs = umty.mattributes }
      in
      (muc, tmty)
  | Mod_ident li ->
      (* module type MTB = *MTA*  module MA : *MTA* *)
      let nm = Longident.flatten_exn li.txt in
      let tmty =
        {
          mt_desc = Mod_ident nm;
          mt_loc = umty.mloc;
          mt_attrs = umty.mattributes;
        }
      in
      let ns = find_tns ~loc:li.loc (get_top_import muc) nm in
      (add_ns_top ~export:true muc ns, tmty)
  | Mod_alias li ->
      (* module MB = *MA* *)
      let nm = Longident.flatten_exn li.txt in
      let tmty =
        {
          mt_desc = Mod_alias nm;
          mt_loc = umty.mloc;
          mt_attrs = umty.mattributes;
        }
      in
      let ns = find_ns ~loc:li.loc (get_top_import muc) nm in
      (add_ns_top ~export:true muc ns, tmty)
  | Mod_with (umty2, cl) ->
      let ns_init = get_top_import muc in
      (* required to type type decls in constraints *)
      let muc, tmty2 = process_modtype penv muc umty2 in
      let process_constraint (muc, cl) c =
        match c with
        | Wtype (li, tyd) ->
            let tdl =
              type_type_declaration muc.muc_kid muc.muc_crcm ns_init
                Nonrecursive [ tyd ]
            in
            let td = match tdl with [ td ] -> td | _ -> assert false in

            let q = Longident.flatten_exn li.txt in
            let ns = get_top_import muc in
            let ts = find_ts ~loc:li.loc ns q in

            (* check that type symbols are compatible
               TODO there are other checks that need to be performed, for
               now we assume that the file passes the ocaml compiler type checker *)
            if ts_arity ts <> ts_arity td.td_ts then
              W.error ~loc:li.loc
                (W.Bad_type_arity
                   (ts.ts_ident.id_str, ts_arity ts, ts_arity td.td_ts));
            (match (ts.ts_alias, td.td_ts.ts_alias) with
            | None, Some _ -> ()
            | Some ty1, Some ty2 -> ignore (ty_match Mtv.empty ty1 ty2)
            | _ -> assert false);

            let muc = muc_replace_ts muc td.td_ts q in
            let muc = muc_subst_ts muc ts td.td_ts in
            (muc, Wty (ts.ts_ident, td) :: cl)
        | Wtypesubst (li, tyd) ->
            let tdl =
              type_type_declaration muc.muc_kid muc.muc_crcm ns_init
                Nonrecursive [ tyd ]
            in
            let td = match tdl with [ td ] -> td | _ -> assert false in
            let ty =
              match td.td_ts.ts_alias with
              | None -> assert false (* should not happen *)
              | Some ty -> ty
            in

            let q = Longident.flatten_exn li.txt in
            let ns = get_top_import muc in
            let ts = find_ts ~loc:li.loc ns q in
            let muc = muc_rm_ts muc q in

            (* check that type symbols are compatible
               TODO there are other checks that need to be performed, for
               now we assume that the file passes the ocaml compiler type checker *)
            if ts_arity ts <> ts_arity td.td_ts then
              W.error ~loc:li.loc
                (W.Bad_type_arity
                   (ts.ts_ident.id_str, ts_arity ts, ts_arity td.td_ts));
            (match (ts.ts_alias, td.td_ts.ts_alias) with
            | None, Some _ -> ()
            | Some ty1, Some ty2 -> ignore (ty_match Mtv.empty ty1 ty2)
            | _ -> assert false);

            let muc = muc_subst_ty muc ts td.td_ts ty in
            (muc, Wty (ts.ts_ident, td) :: cl)
        | Wmodule (_, _)
        | Wmodsubst (_, _)
        | Wmodtypesubst (_, _)
        | Wmodtype (_, _) ->
            W.error ~loc:umty.mloc (W.Unsupported "`with' module clause")
      in
      let muc, cl = List.fold_left process_constraint (muc, []) cl in
      let tmty =
        {
          mt_desc = Mod_with (tmty2, List.rev cl);
          mt_loc = umty.mloc;
          mt_attrs = umty.mattributes;
        }
      in
      (muc, tmty)
  | Mod_functor (mto, mt) ->
      let nm, mty_arg, loc =
        match mto with
        | Unit -> W.error ~loc:umty.mloc (W.Unsupported "generative functor")
        | Named ({ txt; loc }, mt) -> (Option.value ~default:"_" txt, mt, loc)
      in
      let muc = open_module muc nm in
      let muc, tmty_arg = process_modtype penv muc mty_arg in
      let muc = close_module_functor muc in
      let muc, tmty = process_modtype penv muc mt in
      let tmty =
        {
          mt_desc = Mod_functor (Ident.create ~loc nm, Some tmty_arg, tmty);
          mt_loc = umty.mloc;
          mt_attrs = umty.mattributes;
        }
      in
      (muc, tmty)
  | Mod_typeof _ -> W.error ~loc:umty.mloc (W.Unsupported "module type of")
  | Mod_extension _ -> W.error ~loc:umty.mloc (W.Unsupported "module extension")

and process_mod penv loc m muc =
  let nm = Option.value ~default:"_" m.mdname.txt in
  let muc = open_module muc nm in
  let muc, mty = process_modtype penv muc m.mdtype in
  let decl =
    {
      md_name = Ident.create ~loc:m.mdname.loc nm;
      md_type = mty;
      md_attrs = m.mdattributes;
      md_loc = m.mdloc;
    }
  in
  (close_module muc, mk_sig_item (Sig_module decl) loc)

and process_modtype_decl penv loc decl muc =
  let nm = decl.mtdname.txt in
  let muc = open_module muc nm in
  let md_mty = Option.map (process_modtype penv muc) decl.mtdtype in
  let muc, mty =
    match md_mty with None -> (muc, None) | Some (muc, mty) -> (muc, Some mty)
  in
  let decl =
    {
      mtd_name = Ident.create ~loc:decl.mtdname.loc nm;
      mtd_type = mty;
      mtd_attrs = decl.mtdattributes;
      mtd_loc = decl.mtdloc;
    }
  in
  (close_module_type muc, mk_sig_item (Sig_modtype decl) loc)

and process_sig_item penv muc { sdesc; sloc } =
  let process_sig_item si muc =
    let kid, ns, crcm = (muc.muc_kid, get_top_import muc, muc.muc_crcm) in
    match si with
    | Uast.Sig_type (r, tdl) ->
        (muc, process_sig_type ~loc:sloc kid crcm ns r tdl)
    | Uast.Sig_val vd -> (muc, process_val ~loc:sloc kid crcm ns vd)
    | Uast.Sig_typext te -> (muc, mk_sig_item (Sig_typext te) sloc)
    | Uast.Sig_module m -> process_mod penv sloc m muc
    | Uast.Sig_recmodule _ -> W.error ~loc:sloc (W.Unsupported "module rec")
    | Uast.Sig_modsubst _ | Uast.Sig_modtypesubst _ | Uast.Sig_typesubst _ ->
        W.error ~loc:sloc (W.Unsupported "type substitution")
    | Uast.Sig_modtype mty_decl -> process_modtype_decl penv sloc mty_decl muc
    | Uast.Sig_exception te -> (muc, process_exception_sig sloc ns te)
    | Uast.Sig_open od -> process_open ~loc:sloc ~ghost:Nonghost penv muc od
    | Uast.Sig_include id -> (muc, mk_sig_item (Sig_include id) sloc)
    | Uast.Sig_class cdl -> (muc, mk_sig_item (Sig_class cdl) sloc)
    | Uast.Sig_class_type ctdl -> (muc, mk_sig_item (Sig_class_type ctdl) sloc)
    | Uast.Sig_attribute a -> (muc, mk_sig_item (Sig_attribute a) sloc)
    | Uast.Sig_extension (e, a) -> (muc, mk_sig_item (Sig_extension (e, a)) sloc)
    | Uast.Sig_function f -> (muc, process_function kid crcm ns f)
    | Uast.Sig_axiom a -> (muc, process_axiom sloc kid crcm ns a)
    | Uast.Sig_ghost_type (r, tdl) ->
        (muc, process_sig_type ~loc:sloc ~ghost:Ghost kid crcm ns r tdl)
    | Uast.Sig_ghost_val vd ->
        (muc, process_val ~loc:sloc ~ghost:Ghost kid crcm ns vd)
    | Uast.Sig_ghost_open od -> process_open ~loc:sloc ~ghost:Ghost penv muc od
  in
  let rec process_and_import si muc =
    try process_sig_item si muc
    with Ns_not_found (loc, s) ->
      (* if a namespace does not exist we try to load
         a file with the same name *)
      let muc = module_as_file ~loc penv muc s in
      let sig_ = mk_sig_item (Sig_use s) sloc in
      let muc = add_sig_contents muc sig_ in
      process_and_import si muc
  in
  let muc, signature = process_and_import sdesc muc in
  let muc = add_sig_contents muc signature in
  (muc, signature)

and type_sig_item penv muc sig_item =
  let muc, _ = process_sig_item penv muc sig_item in
  muc
