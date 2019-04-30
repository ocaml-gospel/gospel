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

let find_q_ts ns q =
  let ln = string_list_of_qualid q in
  try ns_find_ts ns ln with
    Not_found -> error ~loc:(q_loc q) (SymbolNotFound ln)

let find_q_ls ns q =
  let ln = string_list_of_qualid q in
  try ns_find_ls ns ln with
    Not_found -> error ~loc:(q_loc q) (SymbolNotFound ln)

let find_q_ns ns q =
  let ln = string_list_of_qualid q in
  try ns_find_ns ns ln with
    Not_found -> error ~loc:(q_loc q) (SymbolNotFound ln)

(** Typing types *)

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

(** Typing terms *)

open Dterm
open Tterm
open Tast

let dty_of_pty ns dty = dty_of_ty (ty_of_pty ns dty)

(* let parse_record ns fl =
 *   let fl = List.map (fun (q,p) -> find_q_ls ns q,p) fl in
 *   let fs = match fl with
 *     | [] -> assert false (\* empty record *\)
 *     | (fs,_)::_ -> fs in
 *   let ts = match fs.ls_args with
 *     | [{ ty_node = Tyapp (ts,_) }] -> ts
 *     | _ -> assert false (\* bad record field *\) in *)

let rec dpattern ns {pat_desc;pat_loc=loc} =
  let mk_dpattern ?loc dp_node dp_dty dp_vars =
    {dp_node;dp_dty;dp_vars;dp_loc=loc} in
  let mk_papp cs dpl =
    let dtyl, dty = specialize_cs cs in
    app_unify cs dpattern_unify dpl dtyl;
    let check_duplicate s _ _ = raise (DuplicatedVar s) in
    let vars = List.fold_left (fun acc dp ->
      Mstr.union check_duplicate acc dp.dp_vars) Mstr.empty dpl in
    mk_dpattern ~loc (DPapp (cs,dpl)) dty vars
  in
  match pat_desc with
  | Pwild ->
     let dty = dty_fresh () in
     mk_dpattern ~loc DPwild dty Mstr.empty
  | Pvar pid ->
     let dty = dty_fresh () in
     let vars = Mstr.singleton pid.pid_str dty in
     mk_dpattern ~loc (DPvar pid) dty vars
  | Papp (q,pl) ->
     let cs = find_q_ls ns q in
     let dpl = List.map (dpattern ns) pl in
     mk_papp cs dpl
  | Ptuple pl ->
     let cs = fs_tuple (List.length pl) in
     let dpl = List.map (dpattern ns) pl in
     mk_papp cs dpl
  | Pas (p,pid) ->
     let dp = dpattern ns p in
     if Mstr.mem pid.pid_str dp.dp_vars
       then raise (DuplicatedVar pid.pid_str);
     let vars = Mstr.add pid.pid_str dp.dp_dty dp.dp_vars in
     mk_dpattern ~loc (DPas (dp,pid)) dp.dp_dty vars
  | Por (p1,p2) ->
     let dp1 = dpattern ns p1 in
     let dp2 = dpattern ns p2 in
     dpattern_unify dp1 dp2.dp_dty;
     let join _ dty1 dty2 =
       dty_unify ?loc:dp1.dp_loc dty1 dty2; Some dty1 in
     let vars = Mstr.union join dp1.dp_vars dp2.dp_vars in
     mk_dpattern ~loc (DPor (dp1,dp2)) dp1.dp_dty vars
  | Pcast (p,pty) ->
     let dp = dpattern ns p in
     let dty = dty_of_pty ns pty in
     dpattern_unify dp dty;
     mk_dpattern ~loc (DPcast (dp,dty)) dty dp.dp_vars
  | Prec qpl -> assert false

let binop = function
  | Uast.Tand -> Tand | Uast.Tand_asym -> Tand_asym
  | Uast.Tor -> Tor | Uast.Tor_asym -> Tor_asym
  | Uast.Timplies -> Timplies | Uast.Tiff -> Tiff

let quant = function
  | Uast.Tforall -> Tforall | Uast.Texists -> Tforall
  | Uast.Tlambda -> Tlambda

exception PartialApplication of lsymbol

let rec dterm ns denv {term_desc;term_loc=loc}: dterm =
  let mk_dterm ?(loc=Some loc) dt_node dty =
    {dt_node;dt_dty=dty;dt_loc=loc} in
  let map_apply dt1 t2 =
    let dt2 = dterm ns denv t2 in
    let dty = dty_fresh () in
    unify dt1 (Some (Tapp (ts_arrow, [dty_of_dterm dt2;dty])));
    let dt_app = DTapp (fs_fun_apply, [dt1;dt2]) in
    mk_dterm ~loc:dt2.dt_loc dt_app (Some dty) in (* CHECK location *)
  let fun_app ls tl =
    if List.length tl < List.length ls.ls_args
      then raise (PartialApplication ls);
    let params, extra = split_at_i (List.length ls.ls_args) tl in
    let params = List.map (dterm ns denv) params in
    let dtyl, dty = specialize_ls ls in
    app_unify ls dterm_unify params dtyl;
    let dt1 = mk_dterm (DTapp (ls,params)) dty in
    if extra = [] then dt1 else
      List.fold_left map_apply dt1 extra in
  let rec unfold_map t1 t2 dtl = match t1.term_desc with
    | Uast.Tpreid q ->
       let ls = find_q_ls ns q in
       fun_app ls (t2::dtl)
    | Uast.Tapply (t11,t12) -> unfold_map t11 t12 (t2::dtl)
    | _ -> let dt1 = dterm ns denv t1 in
           List.fold_left map_apply dt1 (t2::dtl) in
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
     let dty = denv_find pid.pid_str denv in
     mk_dterm (DTvar pid) (Some dty)
  | Uast.Tpreid q -> (* in this case it must be a constant *)
     let ls = find_q_ls ns q in
     if List.length ls.ls_args > 0 then
       error ~loc (PartialApplication ls);
     let _, dty = specialize_ls ls in
     let node,dty = DTapp (ls,[]), dty in
     mk_dterm node dty
  | Uast.Tidapp (q,tl) ->
     let ls = find_q_ls ns q in
     fun_app ls tl
  | Uast.Tapply  (t1,t2) ->
     unfold_map t1 t2 []
  | Uast.Tnot t ->
     let dt = dterm ns denv t in
     dfmla_unify dt;
     mk_dterm  (DTnot dt) (dt.dt_dty)
  | Uast.Tif (t1,t2,t3) ->
     let dt1 = dterm ns denv t1 in
     let dt2 = dterm ns denv t2 in
     let dt3 = dterm ns denv t3 in
     dfmla_unify dt1;
     unify dt2 dt3.dt_dty;
     mk_dterm (DTif (dt1,dt2,dt3)) dt2.dt_dty
  | Uast.Ttuple tl ->
     fun_app (fs_tuple (List.length tl)) tl
  | Uast.Tlet (pid,t1,t2) ->
     let dt1 = dterm ns denv t1 in
     let denv = denv_add_var denv pid.pid_str (dty_of_dterm dt1) in
     let dt2 = dterm ns denv t2 in
     mk_dterm  (DTlet (pid,dt1,dt2)) dt2.dt_dty
  | Uast.Tinfix (t1,op,t2) ->
     let op = ns_find_ls ns [op.pid_str] in
     fun_app op [t1;t2]
  | Uast.Tbinop (t1,op,t2) ->
     let dt1 = dterm ns denv t1 in
     let dt2 = dterm ns denv t2 in
     dfmla_unify dt1; dfmla_unify dt2;
     mk_dterm (DTbinop (binop op,dt1,dt2)) None
  | Uast.Tquant (q,vl,trl,t) ->
     let get_dty pty = match pty with
       | None -> dty_fresh ()
       | Some pty -> dty_of_pty ns pty in
     let vl = List.map (fun (pid,pty) -> pid,get_dty pty) vl in
     let denv = denv_add_var_quant denv vl in
     let dtrl = List.map (List.map (dterm ns denv)) trl in
     let dt = dterm ns denv t in
     dfmla_unify dt;
     mk_dterm (DTquant (quant q,vl,dtrl,dt)) None
  | Uast.Tcase (t,ptl) ->
     let dt = dterm ns denv t in
     let dt_dty = dty_of_dterm dt in
     let dty = dty_fresh () in
     let branch (p,t) =
       let dp = dpattern ns p in
       dpattern_unify dp dt_dty;
       let choose_snd _ _ x = Some x in
       let denv = Mstr.union choose_snd denv dp.dp_vars in
       let dt = dterm ns denv t in
       dterm_unify dt dty;
       dp, dt in
     let pdtl = List.map branch ptl in
     mk_dterm (DTcase (dt,pdtl)) (Some dty)
  | Uast.Tcast (t,pty) ->
     let dt = dterm ns denv t in
     let dty = dty_of_pty ns pty in
     dterm_unify dt dty; dt
  | Uast.Tscope (q,t) ->
     let ns = find_q_ns ns q in
     dterm ns denv t
  | Uast.Tattr (at,t) ->
     let dt = dterm ns denv t in
     mk_dterm (DTattr (dt,Sattr.singleton at)) dt.dt_dty
  | Uast.Told t ->
     let dt = dterm ns denv t in
     mk_dterm (DTold dt) dt.dt_dty
  | Uast.Trecord _ -> Format.eprintf "\n\n Trecord \n@."; assert false
  | Uast.Tupdate _ -> Format.eprintf "\n\n Tupdate \n@."; assert false

let term ty ns env t =
  let denv = Mstr.map (fun vs -> dty_of_ty vs.vs_ty) env in
  let dt = dterm ns denv t in
  dterm_unify dt (dty_of_ty ty);
  term env dt

let fmla ns env t =
  let denv = Mstr.map (fun vs -> dty_of_ty vs.vs_ty) env in
  let dt = dterm ns denv t in
  fmla env dt

(** Typing declarations *)


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

exception CyclicTypeDecl of string

(* TODO
   - I'm still not sure how I should threat Ptyp_any
   - compare manifest with td_kind
   - deal with type_spec
 *)
let process_sig_type ~loc ?(ghost=false) r tdl md =
  let add_new tdm ({tname} as td) =
    if Mstr.mem tname.txt tdm then raise (NameClash tname.txt) else
      Mstr.add tname.txt td tdm in
  let tdm = List.fold_left add_new Mstr.empty tdl in
  let hts = Hstr.create 5 in
  let htd = Hstr.create 5 in
  let type_spec spec = {ty_ephemeral = spec.Uast.ty_ephemeral} in
  let open Oparsetree in

  let rec parse_core ~alias tvl core = match core.ptyp_desc with
    | Ptyp_any ->
       fresh_ty_var "_" (* TODO not sure about this - i think we should
                        not support this *)
    | Ptyp_var s -> begin
       try {ty_node = Tyvar (Mstr.find s tvl)} with Not_found ->
         error ~loc:core.ptyp_loc (UnboundVar s) end
    (* TODO use smart-constructor ? *)
    | Ptyp_arrow (lbl,ct1,ct2) ->
       assert (lbl != Nolabel); (* TODO check what to do *)
       let ty1, ty2 = parse_core ~alias tvl ct1,
                      parse_core ~alias tvl ct2 in
       ty_app ts_arrow [ty1;ty2]
    | Ptyp_tuple ctl ->
       let tyl = List.map (parse_core ~alias tvl) ctl in
       ty_app (ts_tuple (List.length tyl)) tyl
    | Ptyp_constr (lid,ctl) ->
       let idl = Longident.flatten lid.txt in
       let tyl = List.map (parse_core ~alias tvl) ctl in
       let ts = match idl with
         | [s] when Sstr.mem s alias ->
            error ~loc:core.ptyp_loc (CyclicTypeDecl s)
         | [s] when Hstr.mem hts s ->
            Hstr.find hts s
         | [s] when Mstr.mem s tdm ->
            visit ~alias s (Mstr.find s tdm);
            Hstr.find hts s
         | s -> find_ts md s in
       if List.length tyl <> ts_arity ts then
         error ~loc:core.ptyp_loc (BadTypeArity (ts, List.length tyl));
       ty_app ts tyl
    | _ -> assert false (* TODO what to do with other cases? keep them
                           as they are? *)

  (* TODO in the style of why3 use an alias set for the types
     currently visited and an alg for the type symbols known *)
  and visit ~alias s td : unit =
    let id = fresh_id s in
    let td_private = private_flag td.tprivate in
    let td_attrs = td.tattributes and td_loc = td.tloc in
    let td_ts = ts id (List.length td.tparams) in
    Hstr.add hts s td_ts;

    let parse_params (tvl,params,vs) (ct,v) = match ct.ptyp_desc with
      | Ptyp_any -> tvl, tv_of_string "_" :: params, variance v :: vs
      | Ptyp_var s ->
         let tv = tv_of_string s in
         Mstr.add s tv tvl, tv :: params, variance v :: vs
      | _ -> assert false (* should not happen *) in
    let tvl,params,vl =
      List.fold_left parse_params (Mstr.empty,[],[]) td.tparams in
    let params,vl = List.rev params, List.rev vl in
    let ty = ty_app td_ts (List.map ty_of_var params) in
    let td_params = List.combine params vl in

    let alias = Sstr.add s alias in
    let td_manifest = opmap (parse_core ~alias tvl) td.tmanifest; in

    let constraints (ct1,ct2,l) =
      parse_core ~alias tvl ct1, parse_core ~alias tvl ct2,l in
    let td_cstrs = List.map constraints td.tcstrs in

    let alias = Sstr.empty in (* TODO check if this is the right place to do it *)

    let make_rd ?(constr=false) ldl =
      let cs_id = fresh_id ("constr_" ^ s) in
      let fields_ty = List.map (fun ld ->
                          parse_core ~alias tvl ld.pld_type) ldl in
      let rd_cs = fsymbol ~constr cs_id fields_ty ty in
      let mk_ld ld =
        let id = fresh_id ld.pld_name.txt in
        let ty_res = parse_core ~alias tvl ld.pld_type in
        let ld_field = Rec_pj (fsymbol id [ty] ty_res) in
        let ld_mut = mutable_flag ld.pld_mutable in
        {ld_field;ld_mut;ld_loc=ld.pld_loc;
         ld_attrs=ld.pld_attributes} in
      {rd_cs;rd_ldl = List.map mk_ld ldl} in
    let make_cd cd =
      assert (cd.pcd_res = None); (* TODO check what this is*)
      let cs_id = fresh_id cd.pcd_name.txt in
      let cd_cs,cd_ld = match cd.pcd_args with
        | Pcstr_tuple ctl ->
           let tyl = List.map (parse_core ~alias tvl) ctl in
           let arg = match tyl with
             | [] | [_] -> tyl
             | _ -> [ty_app (ts_tuple (List.length tyl)) tyl] in
           fsymbol ~constr:true cs_id arg ty, []
        | Pcstr_record ldl ->
           let add ld (ldl,tyl) =
             let id = fresh_id ld.pld_name.txt in
             let ty = parse_core ~alias tvl ld.pld_type in
             let ld_field = Constr_field (id,ty) in
             let ld_mut = mutable_flag ld.pld_mutable in
             let ld = {ld_field;ld_mut;ld_loc=ld.pld_loc;
                       ld_attrs=ld.pld_attributes} in
             ld :: ldl, ty :: tyl in
           let ldl,tyl = List.fold_right add ldl ([],[]) in
           fsymbol ~constr:true cs_id tyl ty, ldl in
      {cd_cs;cd_ld;cd_loc=cd.pcd_loc;cd_attrs=cd.pcd_attributes} in
    let td_kind = match td.tkind with
      | Ptype_abstract -> Pty_abstract
      | Ptype_variant cdl ->
         Pty_variant (List.map make_cd cdl)
      | Ptype_record ldl -> Pty_record (make_rd ldl)
      | Ptype_open -> assert false in

    let td_spec = type_spec td.tspec in
    let td =
      {td_ts; td_params; td_cstrs; td_kind; td_private;
       td_manifest; td_attrs; td_spec; td_loc} in
    Hstr.add htd s td in

  Mstr.iter (visit ~alias:Sstr.empty) tdm;
  let tdl = List.map (fun td -> Hstr.find htd td.tname.txt) tdl in
  let sig_desc = Sig_type (rec_flag r,tdl,ghost) in
  mk_sig_item sig_desc loc

(** process val *)

let rec val_parse_core_type ns cty =
  let open Oparsetree in
  let rec ty_of_core cty = match cty.ptyp_desc with
    | Ptyp_any ->
       {ty_node = Tyvar (create_tv (fresh_id "_"))}
    | Ptyp_var s ->
       {ty_node = Tyvar (tv_of_string s)}
    | Ptyp_tuple ctl ->
       let tyl = List.map ty_of_core ctl in
       ty_app (ts_tuple (List.length tyl)) tyl
    | Ptyp_constr (lid,ctl) ->
       let ts = ns_find_ts ns (Longident.flatten lid.txt) in
       let tyl = List.map ty_of_core ctl in
       ty_app ts tyl
    | Ptyp_arrow (lbl,ct1,ct2) ->
       (* TODO check what to do with the lbl *)
       let ty1, ty2 = ty_of_core ct1, ty_of_core ct2 in
       ty_app ts_arrow [ty1;ty2]
    | _ -> assert false in
  match cty.ptyp_desc with
  | Ptyp_arrow (lbl,ct1,ct2) ->
     let args, res = val_parse_core_type ns ct2 in
     (ty_of_core ct1,lbl) :: args, res
  | _ -> [], ty_of_core cty

(* Checks the following
   1 - the val id string is equal to the name in val header
   2 - no duplicated names in arguments and arguments in header
   match core type
   3 -
*)
let rec process_val_spec md id cty (vs:Uast.val_spec) =
  check_report ~loc:vs.sp_hd_nm.pid_loc
    (id.id_str = vs.sp_hd_nm.pid_str) "val specification header does \
                                       not match name";

  let args, ret = val_parse_core_type md.mod_ns cty in

  let add_arg la env lal =
    let vs = vs_of_lb_arg la in
    let vs_str = vs.vs_name.id_str in
    let add = function
      | None -> Some vs
      | Some s -> raise (DuplicatedVar vs_str) in
    Mstr.update vs_str add env, la :: lal in

  let rec process_args args tyl env lal = match args, tyl with
    | [], [] -> env, List.rev lal
    | [], _ -> error_report ~loc:(vs.sp_hd_nm.pid_loc) "too few parameters"
    | Uast.Lghost (pid,pty) :: args, _ ->
       let ty = ty_of_pty md.mod_ns pty in
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
         "too many parameters" in

  let env, args = process_args vs.sp_hd_args args Mstr.empty [] in
  let env, ret = match vs.sp_hd_ret, ret.ty_node with
    | [], _ -> env, []
    | _, Tyapp (ts,tyl) when is_ts_tuple ts ->
       let tyl = List.map (fun ty -> (ty,Oasttypes.Nolabel)) tyl in
       process_args vs.sp_hd_ret tyl env []
    | _, _ -> process_args vs.sp_hd_ret [(ret,Oasttypes.Nolabel)] env [] in

  let pre = List.map (fun (t,c) ->
                   fmla md.mod_ns env t, c) vs.sp_pre in
  let post = List.map (fmla md.mod_ns env) vs.sp_post in
  let writes = List.map (fmla md.mod_ns env) vs.sp_writes in

  mk_val_spec args ret pre post writes vs.sp_diverge vs.sp_equiv

let process_val ~loc ?(ghost=false) vd md =
  let id = id_add_loc vd.vname.loc (fresh_id vd.vname.txt) in
  let spec = opmap (process_val_spec md id vd.vtype) vd.vspec in
  let vd =
    mk_val_description id vd.vtype vd.vprim vd.vattributes spec vd.vloc in
  mk_sig_item (Sig_val (vd,ghost)) loc

(* Currently checking:
   1 - arguments have different names *)
let process_function ns f =
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
    | Some s -> raise (DuplicatedVar nm) in
  let env = List.fold_left (fun env vs ->
    let nm = vs.vs_name.id_str in
    Mstr.update nm (add_var nm vs) env) Mstr.empty params in
  let env = match f_ty with
    | None -> env
    | Some ty ->
       let result = create_vsymbol (pid_of_string "result") ty in
       Mstr.add "result" result env in

  let def = match f_ty with
    | None -> opmap (fmla ns env) f.fun_def
    | Some ty -> opmap (term ty ns env) f.fun_def in

  let spec =
    let req = List.map (fmla ns env) f.fun_spec.fun_req in
    let ens = List.map (fmla ns env) f.fun_spec.fun_ens in
    let variant = List.map (term ty_integer ns env) f.fun_spec.fun_variant in
    mk_fun_spec req ens variant f.fun_spec.fun_coer in

  let f = mk_function ls f.fun_rec params def spec f.fun_loc in
  mk_sig_item (Sig_function f) f.fun_loc

let process_axiom loc ns a =
  let id = id_register a.Uast.ax_name in
  let t  = fmla ns Mstr.empty a.Uast.ax_term in
  let ax = mk_axiom id t a.ax_loc in
  mk_sig_item (Sig_axiom ax) loc

exception EmptyUse

let rec process_use loc md q =
  let f = string_list_of_qualid q in
  let file = match f with
    | []     -> error ~loc EmptyUse
    | f :: m -> String.uncapitalize_ascii f ^ ".mli" in
  let sl = Parser_frontend.parse_all file in
  let f_md = List.fold_left process_signature (md_with_primitives file) sl in
  add_ns_to_md f_md.mod_ns md, mk_sig_item (Sig_use f) loc

and process_signature md {sdesc;sloc} =
  let md, signature = match sdesc with
  | Uast.Sig_type (r,tdl)    -> md, process_sig_type ~loc:sloc r tdl md
  | Uast.Sig_val vd          -> md, process_val ~loc:sloc vd md
  | Uast.Sig_typext te       -> md, mk_sig_item (Sig_typext te) sloc
  | Uast.Sig_module m        -> md, mk_sig_item (Sig_module m) sloc
  | Uast.Sig_recmodule ml    -> md, mk_sig_item (Sig_recmodule ml) sloc
  | Uast.Sig_modtype ml      -> md, mk_sig_item (Sig_modtype ml) sloc
  | Uast.Sig_exception te    -> md, mk_sig_item (Sig_exception te) sloc
  | Uast.Sig_open od         -> md, mk_sig_item (Sig_open od) sloc
  | Uast.Sig_include id      -> md, mk_sig_item (Sig_include id) sloc
  | Uast.Sig_class cdl       -> md, mk_sig_item (Sig_class cdl) sloc
  | Uast.Sig_class_type ctdl -> md, mk_sig_item (Sig_class_type ctdl) sloc
  | Uast.Sig_attribute a     -> md, mk_sig_item (Sig_attribute a) sloc
  | Uast.Sig_extension (e,a) -> md, mk_sig_item (Sig_extension (e,a)) sloc
  | Uast.Sig_use q           -> process_use sloc md q
  | Uast.Sig_function f      -> md, process_function md.mod_ns f
  | Uast.Sig_axiom a         -> md, process_axiom sloc md.mod_ns a
  | Uast.Sig_ghost_type (r,tdl) ->
     md, process_sig_type ~loc:sloc ~ghost:true r tdl md
  | Uast.Sig_ghost_val vd    -> md, process_val ~loc:sloc ~ghost:true vd md
  in add_sig_contents md signature

let () =
  let open Location in
  register_error_of_exn (function
      | PartialApplication ls ->
         Some (errorf "Symbol %a cannot be partially applied" print_ls_nm ls)
      | SymbolNotFound sl ->
         Some (errorf "Symbol %s not found" (String.concat "." sl))
      | EmptyUse ->
         Some (errorf "Empty use")
      | _ -> None)
