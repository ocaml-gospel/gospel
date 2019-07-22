open Utils
open Identifier
open Uast
open Ttypes
open Tmodule

(** Utils *)

let rec string_list_of_qualid q =
  let rec fold_q acc = function
    | Qpreid pid -> pid.pid_str :: acc
    | Qdot (q,pid) -> fold_q (pid.pid_str :: acc) q in
  fold_q [] q

exception SymbolNotFound of string list

let rec q_loc = function
  | Qpreid pid -> pid.pid_loc
  | Qdot (q,p) -> q_loc q

let ns_find ?loc f ns s =
  try f ns s with
    Not_found -> error ?loc (SymbolNotFound s)

let find_ts ?loc = ns_find ?loc ns_find_ts
let find_ls ?loc = ns_find ?loc ns_find_ls
let find_xs ?loc = ns_find ?loc ns_find_xs
let find_ns ?loc = ns_find ?loc ns_find_ns
let find_tns ?loc = ns_find ?loc ns_find_tns

let find_q (f:?loc:Location.t -> 'a) ns q =
  let ln = string_list_of_qualid q in
  f ~loc:(q_loc q) ns ln

let find_q_ts = find_q find_ts
let find_q_ls = find_q find_ls
let find_q_xs = find_q find_xs
let find_q_ns = find_q find_ns

(** Typing types *)

(* specification types *)
let rec ty_of_pty ns = function
  | PTtyvar {pid_str} ->
     (* CHECK following what's done in why3, attributes are ignored*)
     {ty_node = Tyvar (tv_of_string pid_str)}
  | PTtyapp (q,ptyl) ->
     let ts = find_q_ts ns q in
     ty_app ts (List.map (ty_of_pty ns) ptyl)
  | PTtuple ptyl ->
     let tyl = List.map (ty_of_pty ns) ptyl in
     let ts = ts_tuple (List.length tyl) in
     ty_app ts tyl
  | PTarrow (_,pty1,pty2) ->
     let ty1, ty2 = ty_of_pty ns pty1, ty_of_pty ns pty2 in
     ty_app ts_arrow [ty1;ty2]

(* OCaml types *)
let rec ty_of_core ns cty =
  let open Oparsetree in
  match cty.ptyp_desc with
  | Ptyp_any ->
     {ty_node = Tyvar (create_tv (fresh_id "_"))}
  | Ptyp_var s ->
     {ty_node = Tyvar (tv_of_string s)}
  | Ptyp_tuple ctl ->
     let tyl = List.map (ty_of_core ns) ctl in
     ty_app (ts_tuple (List.length tyl)) tyl
  | Ptyp_constr (lid,ctl) ->
     let ts = find_ts ~loc:lid.loc ns (Longident.flatten lid.txt) in
     let tyl = List.map (ty_of_core ns) ctl in
     ty_app ts tyl
  | Ptyp_arrow (lbl,ct1,ct2) ->
     (* TODO check what to do with the lbl *)
     let ty1, ty2 = (ty_of_core ns) ct1, (ty_of_core ns) ct2 in
     ty_app ts_arrow [ty1;ty2]
  | _ -> assert false

(** Typing terms *)

open Dterm
open Tterm
open Tast

let dty_of_pty ns dty = dty_of_ty (ty_of_pty ns dty)

exception EmptyRecord
exception BadRecordField of lsymbol
exception DuplicateRecordField of lsymbol
exception RecordFieldMissing of lsymbol

let find_constructors kid ts =
  match (Mid.find ts.ts_ident kid).sig_desc with
  | Sig_type (_,tdl,_) -> begin
     match (List.find (fun td -> td.td_ts = ts) tdl).td_kind with
     | Pty_record {rd_cs;rd_ldl} ->
        rd_cs, List.map (fun ld -> ld.ld_field) rd_ldl
     | _ -> assert false
    end
  | _ -> assert false

let parse_record ~loc kid ns fll =
  let fll = List.map (fun (q,v) -> find_q_ls ns q,v) fll in
  let fs = match fll with
    | [] -> error ~loc EmptyRecord
    | (fs,_)::_ -> fs in
  let ts = match fs.ls_args with
    | [{ ty_node = Tyapp (ts,_) }] -> ts
    | _ -> error ~loc (BadRecordField fs) in
  let cs, pjl = find_constructors kid ts in
  let pjs = Sls.of_list pjl in
  let fll = List.fold_left (fun m (pj,v) ->
    if not (Sls.mem pj pjs) then raise (BadRecordField pj) else
      if Mls.mem pj m then raise (DuplicateRecordField pj) else
        Mls.add pj v m) Mls.empty fll in
  cs,pjl,fll

let rec dpattern kid ns {pat_desc;pat_loc=loc} =
  let mk_dpattern ?loc dp_node dp_dty dp_vars =
    {dp_node;dp_dty;dp_vars;dp_loc=loc} in
  let mk_papp ?loc cs dpl =
    let dtyl, dty = specialize_cs ?loc cs in
    app_unify cs dpattern_unify dpl dtyl;
    let check_duplicate s _ _ = error ?loc (DuplicatedVar s) in
    let vars = List.fold_left (fun acc dp ->
      Mstr.union check_duplicate acc dp.dp_vars) Mstr.empty dpl in
    mk_dpattern ?loc (DPapp (cs,dpl)) dty vars
  in
  let mk_pwild loc dty = mk_dpattern ~loc DPwild dty Mstr.empty in
  match pat_desc with
  | Pwild ->
     let dty = dty_fresh () in
     mk_pwild loc dty
  | Pvar pid ->
     let dty = dty_fresh () in
     let vars = Mstr.singleton pid.pid_str dty in
     mk_dpattern ~loc (DPvar pid) dty vars
  | Papp (q,pl) ->
     let cs = find_q_ls ns q in
     let dpl = List.map (dpattern kid ns) pl in
     mk_papp ~loc cs dpl
  | Ptuple pl ->
     let cs = fs_tuple (List.length pl) in
     let dpl = List.map (dpattern kid ns) pl in
     mk_papp ~loc cs dpl
  | Pas (p,pid) ->
     let dp = dpattern kid ns p in
     if Mstr.mem pid.pid_str dp.dp_vars
       then error ~loc:pid.pid_loc (DuplicatedVar pid.pid_str);
     let vars = Mstr.add pid.pid_str dp.dp_dty dp.dp_vars in
     mk_dpattern ~loc (DPas (dp,pid)) dp.dp_dty vars
  | Por (p1,p2) ->
     let dp1 = dpattern kid ns p1 in
     let dp2 = dpattern kid ns p2 in
     dpattern_unify dp1 dp2.dp_dty;
     let join _ dty1 dty2 =
       dty_unify ?loc:dp1.dp_loc dty1 dty2; Some dty1 in
     let vars = Mstr.union join dp1.dp_vars dp2.dp_vars in
     mk_dpattern ~loc (DPor (dp1,dp2)) dp1.dp_dty vars
  | Pcast (p,pty) ->
     let dp = dpattern kid ns p in
     let dty = dty_of_pty ns pty in
     dpattern_unify dp dty;
     mk_dpattern ~loc (DPcast (dp,dty)) dty dp.dp_vars
  | Prec qpl ->
     let cs,pjl,fll = parse_record ~loc kid ns qpl in
     let get_pattern pj = try dpattern kid ns (Mls.find pj fll) with
       Not_found -> mk_pwild loc (dty_of_ty (opget pj.ls_value)) in
     let dpl = List.map get_pattern pjl in
     mk_papp ~loc cs dpl

let binop = function
  | Uast.Tand -> Tand | Uast.Tand_asym -> Tand_asym
  | Uast.Tor -> Tor | Uast.Tor_asym -> Tor_asym
  | Uast.Timplies -> Timplies | Uast.Tiff -> Tiff

exception PartialApplication of lsymbol

let rec dterm kid ns denv {term_desc;term_loc=loc}: dterm =
  let mk_dterm ?(loc = loc) dt_node dty =
    {dt_node;dt_dty=dty;dt_loc=Some loc} in
  let apply dt1 t2 =
    let dt2 = dterm kid ns denv t2 in
    let dty = dty_fresh () in
    unify dt1 (Some (Tapp (ts_arrow, [dty_of_dterm dt2;dty])));
    let dt_app = DTapp (fs_fun_apply, [dt1;dt2]) in
    mk_dterm ?loc:dt2.dt_loc dt_app (Some dty) in (* CHECK location *)
  let map_apply dt tl = List.fold_left apply dt tl in
  let mk_app ?loc ls dtl =
    let dtyl, dty = specialize_ls ls in
    app_unify ls dterm_unify dtl dtyl;
    mk_dterm ?loc (DTapp (ls,dtl)) dty in
  let fun_app ?loc ls tl =
    if List.length tl < List.length ls.ls_args
      then raise (PartialApplication ls);
    let args, extra = split_at_i (List.length ls.ls_args) tl in
    let dtl = List.map (dterm kid ns denv) args in
    let dt = mk_app ?loc ls dtl in
    if extra = [] then dt else map_apply dt extra in
  let qualid_app q tl = match q with
    | Qpreid ({pid_loc = loc; pid_str = s} as pid) ->
        (match denv_get_opt denv s with
        | Some dty ->
           let dtv =
             mk_dterm ~loc:loc (DTvar pid) (Some dty) in
           map_apply dtv tl
        | None -> fun_app (find_q_ls ns q) tl)
    | _ -> fun_app (find_q_ls ns q) tl in
  let rec unfold_app t1 t2 tl = match t1.term_desc with
    | Uast.Tpreid q -> qualid_app q (t2::tl)
    | Uast.Tapply (t11,t12) -> unfold_app t11 t12 (t2::tl)
    | _ -> let dt1 = dterm kid ns denv t1 in
           map_apply dt1 (t2::tl) in
  match term_desc with
  | Uast.Ttrue -> mk_dterm DTtrue (Some dty_bool)
  | Uast.Tfalse -> mk_dterm DTfalse (Some dty_bool)
  | Uast.Tconst c ->
     let dty = match c with
       | Pconst_integer _ -> dty_integer
       | Pconst_char    _ -> dty_char
       | Pconst_string  _ -> dty_string
       | Pconst_float   _ -> dty_float in
     mk_dterm (DTconst c) (Some dty)
  | Uast.Tpreid (Qpreid pid) when is_in_denv denv pid.pid_str ->
     let dty = denv_find ~loc:pid.pid_loc pid.pid_str denv in
     mk_dterm (DTvar pid) (Some dty)
  | Uast.Tpreid q -> (* in this case it must be a constant *)
     let ls = find_q_ls ns q in
     if List.length ls.ls_args > 0 then
       error ~loc (PartialApplication ls);
     let _, dty = specialize_ls ls in
     let node,dty = DTapp (ls,[]), dty in
     mk_dterm node dty
  | Uast.Tidapp (q,tl) -> qualid_app q tl
  | Uast.Tapply  (t1,t2) ->
     unfold_app t1 t2 []
  | Uast.Tnot t ->
     let dt = dterm kid ns denv t in
     dfmla_unify dt;
     mk_dterm  (DTnot dt) (dt.dt_dty)
  | Uast.Tif (t1,t2,t3) ->
     let dt1 = dterm kid ns denv t1 in
     let dt2 = dterm kid ns denv t2 in
     let dt3 = dterm kid ns denv t3 in
     dfmla_unify dt1;
     unify dt2 dt3.dt_dty;
     mk_dterm (DTif (dt1,dt2,dt3)) dt2.dt_dty
  | Uast.Ttuple tl ->
     fun_app (fs_tuple (List.length tl)) tl
  | Uast.Tlet (pid,t1,t2) ->
     let dt1 = dterm kid ns denv t1 in
     let denv = denv_add_var denv pid.pid_str (dty_of_dterm dt1) in
     let dt2 = dterm kid ns denv t2 in
     mk_dterm  (DTlet (pid,dt1,dt2)) dt2.dt_dty
  | Uast.Tinfix (t1,op1,t23) ->
     let apply de1 op de2 =
       let symbol =
         if op.pid_str = neq.id_str then eq.id_str else op.pid_str in
       let ls = find_ls ~loc:op1.pid_loc ns [symbol] in
       let dtyl, dty = specialize_ls ls in
       app_unify ls dterm_unify [de1;de2] dtyl;
       if op.pid_str = neq.id_str then
         mk_dterm  (DTnot (mk_dterm (DTapp (ls,[de1;de2])) dty)) None
       else
         mk_dterm (DTapp (ls,[de1;de2])) dty in
     let rec chain loc de1 op1 = function
       | { term_desc = Uast.Tinfix (t2, op2, t3); term_loc = loc23 } ->
          let de2 = dterm kid ns denv t2 in
          (* TODO: improve locations of subterms. See loc_cutoff function in why3 typing.ml *)
          (* let loc12 = loc_cutoff loc loc23 t2.term_loc in *)
          unify de1 de2.dt_dty;
          let de12 = apply de1 op1 de2 in
          let de23 = chain loc23 de2 op2 t3 in
          mk_dterm (DTbinop (Tand, de12, de23)) None
       | e23 ->
          apply de1 op1 (dterm kid ns denv e23) in
     chain loc (dterm kid ns denv t1) op1 t23
  | Uast.Tbinop (t1,op,t2) ->
     let dt1 = dterm kid ns denv t1 in
     let dt2 = dterm kid ns denv t2 in
     dfmla_unify dt1; dfmla_unify dt2;
     mk_dterm (DTbinop (binop op,dt1,dt2)) None
  | Uast.Tquant (q,vl,trl,t) ->
     let get_dty pty = match pty with
       | None -> dty_fresh ()
       | Some pty -> dty_of_pty ns pty in
     let vl = List.map (fun (pid,pty) -> pid,get_dty pty) vl in
     let denv = denv_add_var_quant denv vl in
     let dtrl = List.map (List.map (dterm kid ns denv)) trl in
     let dt = dterm kid ns denv t in
     let dty, q = match q with
     | Uast.Tforall -> dfmla_unify dt; None, Tforall
     | Uast.Texists -> dfmla_unify dt; None, Texists
     | Uast.Tlambda ->
        let dty = opget_def dty_bool dt.dt_dty in
        let apply (_,dty1) dty2 = Dterm.Tapp (ts_arrow,[dty1;dty2]) in
        Some (List.fold_right apply vl dty), Tlambda in
     mk_dterm (DTquant (q,vl,dtrl,dt)) dty
  | Uast.Tcase (t,ptl) ->
     let dt = dterm kid ns denv t in
     let dt_dty = dty_of_dterm dt in
     let dty = dty_fresh () in
     let branch (p,t) =
       let dp = dpattern kid ns p in
       dpattern_unify dp dt_dty;
       let choose_snd _ _ x = Some x in
       let denv = Mstr.union choose_snd denv dp.dp_vars in
       let dt = dterm kid ns denv t in
       dterm_unify dt dty;
       dp, dt in
     let pdtl = List.map branch ptl in
     mk_dterm (DTcase (dt,pdtl)) (Some dty)
  | Uast.Tcast (t,pty) ->
     let dt = dterm kid ns denv t in
     let dty = dty_of_pty ns pty in
     dterm_unify dt dty; dt
  | Uast.Tscope (q,t) ->
     let ns = find_q_ns ns q in
     dterm kid ns denv t
  | Uast.Tattr (at,t) ->
     let dt = dterm kid ns denv t in
     mk_dterm (DTattr (dt,Sattr.singleton at)) dt.dt_dty
  | Uast.Told t ->
     let dt = dterm kid ns denv t in
     mk_dterm (DTold dt) dt.dt_dty
  | Uast.Trecord qtl ->
     let cs,pjl,fll = parse_record ~loc kid ns qtl in
     let get_term pj = try dterm kid ns denv (Mls.find pj fll) with
       Not_found -> error ~loc (RecordFieldMissing pj) in
     mk_app ~loc cs (List.map get_term pjl)
  | Uast.Tupdate (t,qtl) ->
     let cs, pjl, fll = parse_record ~loc kid ns qtl in
     let get_term pj = try dterm kid ns denv (Mls.find pj fll)
       with Not_found -> fun_app ~loc:t.term_loc pj [t] in
     mk_app ~loc:t.term_loc cs (List.map get_term pjl)

let dterm kid ns env t =
  let denv = Mstr.map (fun vs -> dty_of_ty vs.vs_ty) env in
  dterm kid ns denv t

let term_with_unify kid ty ns env t =
  let dt = dterm kid ns env t in
  dterm_unify dt (dty_of_ty ty);
  term env dt

let fmla kid ns env t =
  let dt = dterm kid ns env t in
  fmla env dt

(** Typing type declarations *)

let private_flag = function
  | Oasttypes.Private -> Private
  | Oasttypes.Public -> Public

let variance = function
  | Oasttypes.Covariant -> Covariant
  | Oasttypes.Contravariant -> Contravariant
  | Oasttypes.Invariant -> Invariant

let rec_flag = function
  | Oasttypes.Nonrecursive -> Nonrecursive
  | Oasttypes.Recursive -> Recursive

let mutable_flag = function
  | Oasttypes.Mutable -> Mutable
  | Oasttypes.Immutable -> Immutable

let process_type_spec kid ns ty (spec:Uast.type_spec) =
  let field (ns,fields) f =
    let f_ty = ty_of_pty ns f.f_pty in
    let ls = fsymbol (id_register f.f_preid) [ty] f_ty in
    let ls_inv = fsymbol (id_register f.f_preid) [] f_ty in
    (ns_add_ls ns f.f_preid.pid_str ls_inv, ls::fields) in
  let (ns,fields) = List.fold_left field (ns,[]) spec.ty_field in
  let fields = List.rev fields in
  let env = Mstr.empty in
  let invariant = List.map (fmla kid ns env) spec.ty_invariant in
  type_spec spec.ty_ephemeral fields invariant

exception CyclicTypeDecl of string

(* TODO compare manifest with td_kind *)
let type_type_declaration ~loc kid ns tdl =
  let add_new tdm td =
    if Mstr.mem td.tname.txt tdm then raise (NameClash td.tname.txt) else
      Mstr.add td.tname.txt td tdm in
  let tdm = List.fold_left add_new Mstr.empty tdl in
  let hts = Hstr.create 5 in
  let htd = Hstr.create 5 in
  let open Oparsetree in

  let rec parse_core alias tvl core = match core.ptyp_desc with
    | Ptyp_any ->
       not_supported ~loc:core.ptyp_loc "_ type parameters not supported yet"
    | Ptyp_var s -> begin
       try {ty_node = Tyvar (Mstr.find s tvl)} with Not_found ->
         error ~loc:core.ptyp_loc (UnboundVar s) end
    | Ptyp_arrow (lbl,ct1,ct2) ->
       assert (lbl != Nolabel); (* TODO check what to do *)
       let ty1, ty2 = parse_core alias tvl ct1,
                      parse_core alias tvl ct2 in
       ty_app ts_arrow [ty1;ty2]
    | Ptyp_tuple ctl ->
       let tyl = List.map (parse_core alias tvl) ctl in
       ty_app (ts_tuple (List.length tyl)) tyl
    | Ptyp_constr (lid,ctl) ->
       let idl = Longident.flatten lid.txt in
       let tyl = List.map (parse_core alias tvl) ctl in
       let ts = match idl with
         | [s] when Sstr.mem s alias ->
            error ~loc:core.ptyp_loc (CyclicTypeDecl s)
         | [s] when Hstr.mem hts s ->
            Hstr.find hts s
         | [s] when Mstr.mem s tdm ->
            visit ~alias s (Mstr.find s tdm);
            Hstr.find hts s
         | s -> find_ts ~loc:lid.loc ns s in
       if List.length tyl <> ts_arity ts then
         error ~loc:core.ptyp_loc (BadTypeArity (ts, List.length tyl));
       ty_app ts tyl
    | _ -> assert false (* TODO what to do with other cases? *)

  and visit ~alias s td =

    let parse_params (ct,v) (tvl,params,vs) = match ct.ptyp_desc with
      | Ptyp_var s ->
         let tv = tv_of_string s in
         Mstr.add s tv tvl, tv :: params, variance v :: vs
      | Ptyp_any ->
         not_supported ~loc:ct.ptyp_loc "_ type parameters not supported yet"
      | _ -> assert false (* should not happen -- see parser optional_type_variable *)
    in

    let tvl,params,variance_list =
      List.fold_right parse_params td.tparams (Mstr.empty,[],[]) in

    let manifest = opmap (parse_core (Sstr.add s alias) tvl) td.tmanifest in
    let td_ts = mk_ts (fresh_id s) params manifest in
    Hstr.add hts s td_ts;

    let process_record ?(constr=false) ty alias ldl =
      let cs_id = fresh_id ("constr#" ^ s) in
      let fields_ty = List.map (fun ld ->
                          parse_core alias tvl ld.pld_type) ldl in
      let rd_cs = fsymbol ~constr:true cs_id fields_ty ty in
      let mk_ld ld =
        let id = fresh_id ld.pld_name.txt in
        let ty_res = parse_core alias tvl ld.pld_type in
        let field = fsymbol id [ty] ty_res in
        let mut = mutable_flag ld.pld_mutable in
        label_declaration field mut ld.pld_loc ld.pld_attributes in
      {rd_cs;rd_ldl = List.map mk_ld ldl}
    in

    let process_variant ty alias cd =
      if cd.pcd_res != None then
        not_supported ~loc:cd.pcd_loc "type in constructors not supported";
      let cs_id = fresh_id cd.pcd_name.txt in
      let cd_cs,cd_ld = match cd.pcd_args with
        | Pcstr_tuple ctl ->
           let tyl = List.map (parse_core alias tvl) ctl in
           let arg = match tyl with
             | [] | [_] -> tyl
             | _ -> [ty_app (ts_tuple (List.length tyl)) tyl] in
           fsymbol ~constr:true cs_id arg ty, []
        | Pcstr_record ldl ->
           let add ld (ldl,tyl) =
             let id = fresh_id ld.pld_name.txt in
             let ty = parse_core alias tvl ld.pld_type in
             let field = id,ty in
             let mut = mutable_flag ld.pld_mutable in
             let loc, attrs =  ld.pld_loc, ld.pld_attributes in
             let ld = label_declaration field mut loc attrs in
             ld :: ldl, ty :: tyl in
           let ldl,tyl = List.fold_right add ldl ([],[]) in
           fsymbol ~constr:true cs_id tyl ty, ldl in
      constructor_decl cd_cs cd_ld cd.pcd_loc cd.pcd_attributes
    in

    let ty = ty_app td_ts (List.map ty_of_var params) in
    let kind =
      let alias = Sstr.empty in
      match td.tkind with
      | Ptype_abstract -> Pty_abstract
      | Ptype_variant cdl ->
         Pty_variant (List.map (process_variant ty alias) cdl)
      | Ptype_record ldl ->
         Pty_record (process_record ty alias ldl)
      | Ptype_open -> assert false
    in

    let params = List.combine params variance_list in
    let spec = process_type_spec kid ns ty td.tspec in

    if  td.tcstrs != [] then
      not_supported ~loc:td.tloc "type constraints not supported";

    let td = type_declaration td_ts params [] kind
               (private_flag td.tprivate) manifest
               td.tattributes spec td.tloc in
    Hstr.add htd s td in

  Mstr.iter (visit ~alias:Sstr.empty) tdm;
  List.map (fun td -> Hstr.find htd td.tname.txt) tdl

let process_sig_type ~loc ?(ghost=false) kid ns r tdl =
  let tdl = type_type_declaration ~loc kid ns tdl in
  let sig_desc = Sig_type (rec_flag r,tdl,ghost) in
  mk_sig_item sig_desc loc

(** Type val declarations *)

let rec val_parse_core_type ns cty =
  let open Oparsetree in
  match cty.ptyp_desc with
  | Ptyp_arrow (lbl,ct1,ct2) ->
     let args, res = val_parse_core_type ns ct2 in
     (ty_of_core ns ct1,lbl) :: args, res
  | _ -> [], ty_of_core ns cty

(* Checks the following
   1 - the val id string is equal to the name in val header
   2 - no duplicated names in arguments and arguments in header
   match core type
   3 -
*)
let rec process_val_spec kid ns id cty vs =
  check_report ~loc:vs.sp_hd_nm.pid_loc
    (id.id_str = vs.sp_hd_nm.pid_str) "val specification header does \
                                       not match name";

  let args, ret = val_parse_core_type ns cty in

  let add_arg la env lal =
    let vs = vs_of_lb_arg la in
    let vs_str = vs.vs_name.id_str in
    let add = function
      | None -> Some vs
      | Some s -> error ~loc:vs.vs_name.id_loc (DuplicatedVar vs_str) in
    Mstr.update vs_str add env, la :: lal in

  let rec process_args args tyl env lal = match args, tyl with
    | [], [] -> env, List.rev lal
    | [], _ -> error_report ~loc:(vs.sp_hd_nm.pid_loc) "too few parameters"
    | Uast.Lghost (pid,pty) :: args, _ ->
       let ty = ty_of_pty ns pty in
       let vs = create_vsymbol pid ty in
       let env, lal = add_arg (Lghost vs) env lal in
       process_args args tyl env lal
    | (Lquestion pid)::args, (ty,Oasttypes.Optional s)::tyl ->
       check_report ~loc:pid.pid_loc (pid.pid_str = s)
         "parameter do not match with val type";
       let ty = ty_app ts_option [ty] in
       let vs = create_vsymbol pid ty in
       let env, lal = add_arg (Lquestion vs) env lal in
       process_args args tyl env lal
    | (Lnamed pid)::args, (ty,Oasttypes.Labelled s)::tyl ->
       check_report ~loc:pid.pid_loc (pid.pid_str = s)
         "parameter do not match with val type";
       let vs = create_vsymbol pid ty in
       let env, lal = add_arg (Lnamed vs) env lal in
       process_args args tyl env lal
    | (Lnone pid)::args, (ty,Oasttypes.Nolabel)::tyl ->
       let vs = create_vsymbol pid ty in
       let env, lal = add_arg (Lnone vs) env lal in
       process_args args tyl env lal
    | la::_, _ ->
       error_report ~loc:((Uast_utils.pid_of_label la).pid_loc)
         "parameter do not match with val type" in

  let env, args = process_args vs.sp_hd_args args Mstr.empty [] in

  let pre   = List.map (fun (t,c) ->
                  fmla kid ns env t, c) vs.sp_pre in

  let wr = List.map (fun t -> let dt = dterm kid ns env t in
                              term env dt) vs.sp_writes in

  let process_xpost mxs (loc,exn) =
    let merge_xpost t tl = match t, tl with
      | None, None -> Some []
      | None, Some tl -> Some tl
      | Some t, None -> Some [t]
      | Some t, Some tl -> Some (t::tl) in
    let process mxs (q,pt) =
      let xs = find_q_xs ns q in
      match pt with
      | None -> Mxs.update xs (merge_xpost None) mxs
      | Some (p,t) ->
         let dp = dpattern kid ns p in
         let ty = match p.pat_desc, xs.xs_type with
           | Pvar vs, Exn_tuple [ty] -> ty
           | Ptuple pl, Exn_tuple tyl ->
              ty_app (ts_tuple (List.length tyl)) tyl
           | Prec _, Exn_record _ -> (* TODO unify types and field names *)
              error_report ~loc "exception pattern doesn't match its type"
           | _, _ ->
              error_report ~loc "exception pattern doesn't match its type" in
         dpattern_unify dp (dty_of_ty ty);
         let p,vars = pattern dp in
         let choose_snd _ _ vs = Some vs in
         let env = Mstr.union choose_snd env vars in
         let t = fmla kid ns env t in
         Mxs.update xs (merge_xpost (Some (p,t))) mxs in
    List.fold_left process mxs exn in
  let xpost = List.fold_left process_xpost Mxs.empty vs.sp_xpost in

  let env, ret = match vs.sp_hd_ret, ret.ty_node with
    | [], _ -> env, []
    | _, Tyapp (ts,tyl) when is_ts_tuple ts ->
       let tyl = List.map (fun ty -> (ty,Oasttypes.Nolabel)) tyl in
       process_args vs.sp_hd_ret tyl env []
    | _, _ ->
       process_args vs.sp_hd_ret [(ret,Oasttypes.Nolabel)] env [] in
  let post = List.map (fmla kid ns env) vs.sp_post in

  mk_val_spec args ret pre post xpost wr vs.sp_diverge vs.sp_equiv

let process_val ~loc ?(ghost=false) kid ns vd =
  let id = id_add_loc vd.vname.loc (fresh_id vd.vname.txt) in
  let spec = opmap (process_val_spec kid ns id vd.vtype) vd.vspec in
  let vd =
    mk_val_description id vd.vtype vd.vprim vd.vattributes spec vd.vloc in
  mk_sig_item (Sig_val (vd,ghost)) loc

(** Typing function, axiom, and exception declarations *)

(* Currently checking:
   1 - arguments have different names *)
let process_function kid ns f =
  let f_ty = opmap (ty_of_pty ns) f.fun_type in

  let params = List.map (fun (_,pid,pty) ->
    create_vsymbol pid (ty_of_pty ns pty)) f.fun_params in
  let tyl = List.map (fun vs -> vs.vs_ty) params in

  let ls = lsymbol (id_register f.fun_name) tyl f_ty in
  let ns = if f.fun_rec then ns_add_ls ns f.fun_name.pid_str ls else ns in

  (* check that there is no duplicated parameters; we must do this
     here, before creating identifiers *)
  let add_var nm vs = function
    | None -> Some vs
    | Some s -> error ~loc:vs.vs_name.id_loc (DuplicatedVar nm) in
  let env = List.fold_left (fun env vs ->
    let nm = vs.vs_name.id_str in
    Mstr.update nm (add_var nm vs) env) Mstr.empty params in
  let env, result = match f_ty with
    | None -> env, None
    | Some ty ->
       let result = create_vsymbol (pid_of_string "result") ty in
       Mstr.add "result" result env, Some result in

  let def = match f_ty with
    | None -> opmap (fmla kid ns env) f.fun_def
    | Some ty -> opmap (term_with_unify kid ty ns env) f.fun_def in

  let spec =
    let req = List.map (fmla kid ns env) f.fun_spec.fun_req in
    let ens = List.map (fmla kid ns env) f.fun_spec.fun_ens in
    let variant =
      List.map (term_with_unify kid ty_integer ns env)
        f.fun_spec.fun_variant in
    mk_fun_spec req ens variant f.fun_spec.fun_coer in
  let f = mk_function ?result ls f.fun_rec params def spec f.fun_loc in
  mk_sig_item (Sig_function f) f.fun_loc

let process_axiom loc kid ns a =
  let id = id_register a.Uast.ax_name in
  let t  = fmla kid ns Mstr.empty a.Uast.ax_term in
  let ax = mk_axiom id t a.ax_loc in
  mk_sig_item (Sig_axiom ax) loc

let process_exception_sig loc ns te =
  let ec = te.Oparsetree.ptyexn_constructor in
  let id = id_add_loc ec.pext_name.loc (fresh_id ec.pext_name.txt) in
  let xs = match ec.pext_kind with
    | Pext_rebind lid ->
       find_xs ~loc:lid.loc ns (Longident.flatten lid.txt)
    | Pext_decl (ca,None) ->
       let args = match ca with
         | Pcstr_tuple ctyl ->
            Exn_tuple (List.map (ty_of_core ns) ctyl)
         | Pcstr_record ldl ->
            let get Oparsetree.{pld_name;pld_type} =
              fresh_id_with_loc pld_name.txt pld_name.loc,
              ty_of_core ns pld_type in
            Exn_record (List.map get ldl) in
       xsymbol id args
    | Pext_decl (_,_) ->
       not_supported ~loc "this type of exceptions declaration is not \
                           supported" in
  let ec = extension_constructor id xs ec.pext_kind
             ec.pext_loc ec.pext_attributes in
  let te = type_exception ec te.Oparsetree.ptyexn_loc
             te.Oparsetree.ptyexn_attributes in
  mk_sig_item (Sig_exception te) loc

(** Typing use, and modules *)

let rec process_use loc md q =
  let f = match string_list_of_qualid q with [q] -> q | _ -> assert false in
  try add_ns md f (find_q_ns (get_top_in_ns md) q)
  with Located (_,SymbolNotFound _) ->
    let file = match [f] with
      | []     -> error_report ~loc "Empty use"
      | f :: m -> String.uncapitalize_ascii f ^ ".mli" in
    let sl = Parser_frontend.parse_all file in
    let md = open_module md f in
    let md = List.fold_left process_signature md sl in
    close_merge_module md

(* assumes that a new namespace has been opened *)
and process_modtype md umty = match umty.mdesc with
  | Mod_signature usig ->
     let md = List.fold_left process_signature md usig in
     let tsig = Mod_signature (get_top_sigs md) in
     let tmty = {mt_desc = tsig; mt_loc = umty.mloc;
                 mt_attrs = umty.mattributes} in
     md, tmty
  | Mod_ident li ->
     (* module type MTB = *MTA*  module MA : *MTA* *)
     let nm = Longident.flatten li.txt in
     let tmty = {mt_desc = Mod_ident nm; mt_loc = umty.mloc;
                 mt_attrs = umty.mattributes} in
     let ns = find_tns ~loc:li.loc (get_top_in_ns md) nm in
     add_ns_top md ns, tmty
  | Mod_alias li ->
     (* module MB = *MA* *)
     let nm = Longident.flatten li.txt in
     let tmty = {mt_desc = Mod_alias nm; mt_loc = umty.mloc;
                 mt_attrs = umty.mattributes} in
     let ns = find_ns ~loc:li.loc (get_top_in_ns md) nm in
     add_ns_top md ns, tmty
  | Mod_with (umty2,cl) ->
     let ns_init =
       get_top_in_ns md in (* required to type type decls in constraints *)
     let md, tmty2 = process_modtype md umty2 in
     let process_constraint (md,cl) c = match c with
       | Wtype (li,tyd) ->
          let tdl = type_type_declaration ~loc:tyd.tloc
                      md.md_kid ns_init [tyd] in
          let td = match tdl with
            | [td] -> td | _ -> assert false in

          let q = Longident.flatten li.txt in
          let ns = get_top_in_ns md in
          let ts = find_ts ~loc:li.loc ns q in

          (* check that type symbols are compatible
             TODO there are other checks that need to be performed, for
             now we assume that the file passes the ocaml compiler type checker *)
          check ~loc:li.loc (ts_arity ts = ts_arity td.td_ts)
            (BadTypeArity (ts,ts_arity td.td_ts));
          begin match ts.ts_alias, td.td_ts.ts_alias with
          | None, Some ty2 -> ()
          | Some ty1, Some ty2 -> ignore(ty_match Mtv.empty ty1 ty2)
          | _ -> assert false end;

          let md = md_replace_ts md td.td_ts q in
          let md = md_subst_ts md ts td.td_ts in
          md, Wty (ts.ts_ident, td) :: cl
       | Wtypesubst (li,tyd) ->
          let tdl = type_type_declaration ~loc:tyd.tloc
                      md.md_kid ns_init [tyd] in
          let td = match tdl with
            | [td] -> td | _ -> assert false in
          let ty = match td.td_ts.ts_alias with
            | None -> assert false (* should not happen *)
            | Some ty -> ty in

          let q = Longident.flatten li.txt in
          let ns = get_top_in_ns md in
          let ts = find_ts ~loc:li.loc ns q in
          let md = md_rm_ts md q in

          (* check that type symbols are compatible
             TODO there are other checks that need to be performed, for
             now we assume that the file passes the ocaml compiler type checker *)
          check ~loc:li.loc (ts_arity ts = ts_arity td.td_ts)
            (BadTypeArity (ts,ts_arity td.td_ts));
          begin match ts.ts_alias, td.td_ts.ts_alias with
          | None, Some ty2 -> ()
          | Some ty1, Some ty2 -> ignore(ty_match Mtv.empty ty1 ty2)
          | _ -> assert false end;

          let md = md_subst_ty md ts td.td_ts ty in
          md, Wty (ts.ts_ident, td) :: cl
       | Wmodule (li1,li2) ->
          not_supported ~loc:li1.loc "with module clause not supported"
       | Wmodsubst (li1,li2) ->
          not_supported ~loc:li1.loc "with module clause not supported"
     in
     let md,cl = List.fold_left process_constraint (md,[]) cl in
     let tmty = {mt_desc = Mod_with (tmty2,List.rev cl); mt_loc = umty.mloc;
                 mt_attrs = umty.mattributes} in
     md, tmty
  | Mod_functor (nm,mto,mt) ->
     let mty_arg = match mto with
       | None -> not_supported ~loc:umty.mloc
                   "at this stage functor type must be provided"
       | Some mt -> mt in
     let md = open_module md nm.txt in
     let md, tmty_arg = process_modtype md mty_arg in
     let md = close_module_functor md in
     let md, tmty = process_modtype md mt in
     let tmty =
       {mt_desc = Mod_functor (fresh_id nm.txt, Some tmty_arg, tmty);
        mt_loc = umty.mloc; mt_attrs = umty.mattributes} in
     md, tmty
  | Mod_typeof me -> assert false
  | Mod_extension e -> assert false

and process_mod loc m md =
  let nm = m.mdname.txt in
  let md = open_module md nm in
  let md, mty = process_modtype md m.mdtype in
  let decl = { md_name = fresh_id nm; md_type = mty;
               md_attrs = m.mdattributes; md_loc = m.mdloc } in
  close_module md, mk_sig_item (Sig_module decl) loc

and process_modtype_decl loc decl md =
  let nm = decl.mtdname.txt in
  let md = open_module md nm in
  let md_mty = opmap (process_modtype md) decl.mtdtype in
  let md, mty = match md_mty with
    | None -> md, None
    | Some (md,mty) -> md, Some mty in
  let decl = {mtd_name = fresh_id nm; mtd_type = mty;
              mtd_attrs = decl.mtdattributes; mtd_loc = decl.mtdloc} in
  close_module_type md, mk_sig_item (Sig_modtype decl) loc

and process_signature md {sdesc;sloc} =
  let kid,ns = md.md_kid, get_top_in_ns md in
  let md, signature = match sdesc with
    | Uast.Sig_type (r,tdl)    -> md, process_sig_type ~loc:sloc kid ns r tdl
    | Uast.Sig_val vd          -> md, process_val ~loc:sloc kid ns vd
    | Uast.Sig_typext te       -> md, mk_sig_item (Sig_typext te) sloc
    | Uast.Sig_module m        -> process_mod sloc m md
    | Uast.Sig_recmodule ml    -> not_supported ~loc:sloc "module rec not supported"
    (* md, mk_sig_item (Sig_recmodule (List.map (process_mod sloc) ml)) sloc *)
    | Uast.Sig_modtype mty_decl-> process_modtype_decl sloc mty_decl md
    | Uast.Sig_exception te    -> md, process_exception_sig sloc ns te
    | Uast.Sig_open od         -> md, mk_sig_item (Sig_open od) sloc
    | Uast.Sig_include id      -> md, mk_sig_item (Sig_include id) sloc
    | Uast.Sig_class cdl       -> md, mk_sig_item (Sig_class cdl) sloc
    | Uast.Sig_class_type ctdl -> md, mk_sig_item (Sig_class_type ctdl) sloc
    | Uast.Sig_attribute a     -> md, mk_sig_item (Sig_attribute a) sloc
    | Uast.Sig_extension (e,a) -> md, mk_sig_item (Sig_extension (e,a)) sloc
    | Uast.Sig_use q           ->
       process_use sloc md q,
       mk_sig_item (Sig_use (string_list_of_qualid q)) sloc
    | Uast.Sig_function f      -> md, process_function kid ns f
    | Uast.Sig_axiom a         -> md, process_axiom sloc kid ns a
    | Uast.Sig_ghost_type (r,tdl) ->
       md, process_sig_type ~loc:sloc ~ghost:true kid ns r tdl
    | Uast.Sig_ghost_val vd    -> md, process_val ~loc:sloc ~ghost:true kid ns vd
  in add_sig_contents md signature

let () =
  let open Location in
  register_error_of_exn (function
      | PartialApplication ls ->
         Some (errorf "Symbol %a cannot be partially applied" print_ls_nm ls)
      | SymbolNotFound sl ->
         Some (errorf "Symbol %s not found" (String.concat "." sl))
      | EmptyRecord ->
         Some (errorf "Record cannot be empty")
      | BadRecordField ls ->
         Some (errorf "The record field %a does not exist" print_ls_nm ls)
      | DuplicateRecordField ls ->
         Some (errorf "Duplicated record field %a" print_ls_nm ls)
      | _ -> None)
