open Utils
open Identifier
open Uast

(* When a ghost type or val is detected in the spec parser a
   [Ghost_decl] exception is raised*)
exception Ghost_decl

let mk_loc s e = {
  Location.loc_start = s;
  Location.loc_end = e;
  Location.loc_ghost = false;
}

let mk_pid pid s e = create_pid pid [] (mk_loc s e )
let mk_term d s e = { term_desc = d; term_loc = mk_loc s e }
let mk_pat  d s e = { pat_desc  = d; pat_loc  = mk_loc s e }

let get_op   s e = Qpreid (mk_pid (mixfix "[]") s e)
let set_op   s e = Qpreid (mk_pid (mixfix "[<-]") s e)
let sub_op   s e = Qpreid (mk_pid (mixfix "[_.._]") s e)
let above_op s e = Qpreid (mk_pid (mixfix "[_..]") s e)
let below_op s e = Qpreid (mk_pid (mixfix "[.._]") s e)

let id_anonymous loc = create_pid "_" [] loc
let array_get s e =
  Qdot (Qpreid (mk_pid "Array" s e), mk_pid (mixfix "[]") s e)

let dummy_position = Location.none

let fspec_union s1 s2 = {
      fun_req     = s1.fun_req @ s2.fun_req;
      fun_ens     = s1.fun_ens @ s2.fun_ens;
      fun_variant = s1.fun_variant @ s2.fun_variant;
      fun_coer    = s1.fun_coer || s2.fun_coer;
    }

let empty_tspec = {
    ty_ephemeral = false;
    ty_field = [];
    ty_invariant = [];
  }

let tspec_union s1 s2 = {
    ty_ephemeral = s1.ty_ephemeral || s2.ty_ephemeral;
    ty_field = s1.ty_field @ s2.ty_field;
    ty_invariant = s1.ty_invariant @ s2.ty_invariant;
  }

let rev_tspec ts =
  { ts with
    ty_field = List.rev ts.ty_field;
    ty_invariant = List.rev ts.ty_invariant;
  }

let pid_of_label = function
    | Lnone p | Lquestion p | Lnamed p | Lghost (p,_) -> p

let str_of_label l = (pid_of_label l).pid_str

let loc_of_qualid = function
  | Qpreid pid | Qdot (_,pid) -> pid.pid_loc
