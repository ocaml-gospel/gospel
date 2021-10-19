module Ident = Identifier.Ident
open Tterm
open Ttypes

val create_vsymbol : Identifier.Preid.t -> Ttypes.ty -> Tterm.vsymbol

val lsymbol :
  ?constr:bool ->
  field:bool ->
  Ident.t ->
  Ttypes.ty list ->
  Ttypes.ty option ->
  Tterm.lsymbol

val fsymbol :
  ?constr:bool ->
  field:bool ->
  Ident.t ->
  Ttypes.ty list ->
  Ttypes.ty ->
  Tterm.lsymbol

val psymbol : Ident.t -> Ttypes.ty list -> Tterm.lsymbol

val ls_subst_ts :
  Ttypes.tysymbol -> Ttypes.tysymbol -> Tterm.lsymbol -> Tterm.lsymbol

val ls_subst_ty :
  Ttypes.tysymbol ->
  Ttypes.tysymbol ->
  Ttypes.ty ->
  Tterm.lsymbol ->
  Tterm.lsymbol

val ps_equ : Tterm.lsymbol
val fs_unit : Tterm.lsymbol
val fs_bool_true : Tterm.lsymbol
val fs_bool_false : Tterm.lsymbol
val fs_apply : Tterm.lsymbol
val fs_tuple_ids : (Ident.t, Tterm.lsymbol) Hashtbl.t
val fs_tuple : int -> Tterm.lsymbol
val is_fs_tuple : Tterm.lsymbol -> bool
val p_vars : Tterm.pattern -> Tterm.Svs.t

exception FreeVariables of Svs.t

val t_free_vars : Tterm.term -> Tterm.Svs.t

exception TermExpected of term
exception FmlaExpected of term

val t_free_vs_in_set : Tterm.Svs.t -> Tterm.term -> unit
val t_prop : Tterm.term -> Tterm.term
val t_type : term -> ty
val t_ty_check : term -> ty option -> unit

exception BadArity of lsymbol * int
exception PredicateSymbolExpected of lsymbol
exception FunctionSymbolExpected of lsymbol

val ls_arg_inst : lsymbol -> term list -> ty Mtv.t
val ls_app_inst : lsymbol -> term list -> ty option -> ty Mtv.t
val mk_pattern : pattern_node -> ty -> Svs.t -> pattern
val p_wild : ty -> pattern
val p_var : vsymbol -> pattern
val p_app : lsymbol -> pattern list -> ty -> pattern
val p_or : pattern -> pattern -> pattern
val p_as : pattern -> vsymbol -> pattern
val mk_term : term_node -> ty option -> Ocaml_common.Warnings.loc -> term
val t_var : vsymbol -> Ocaml_common.Warnings.loc -> term

val t_const :
  Ppxlib.Parsetree.constant -> ty -> Ocaml_common.Warnings.loc -> term

val t_app :
  lsymbol -> term list -> ty option -> Ocaml_common.Warnings.loc -> term

val t_field : term -> lsymbol -> ty option -> Ocaml_common.Warnings.loc -> term
val t_if : term -> term -> term -> Ocaml_common.Warnings.loc -> term
val t_let : vsymbol -> term -> term -> Ocaml_common.Warnings.loc -> term
val t_case : term -> (pattern * term) list -> Ocaml_common.Warnings.loc -> term
val t_binop : binop -> term -> term -> Ocaml_common.Warnings.loc -> term
val t_not : term -> Ocaml_common.Warnings.loc -> term
val t_old : term -> Ocaml_common.Warnings.loc -> term
val t_true : Ocaml_common.Warnings.loc -> term
val t_false : Ocaml_common.Warnings.loc -> term
val t_attr_set : string list -> term -> term
val t_bool_true : Ocaml_common.Warnings.loc -> term
val t_bool_false : Ocaml_common.Warnings.loc -> term
val t_equ : term -> term -> Ocaml_common.Warnings.loc -> term

val t_neq :
  term -> term -> Ocaml_common.Warnings.loc -> Ocaml_common.Warnings.loc -> term

val f_binop : binop -> term -> term -> Ocaml_common.Warnings.loc -> term
val f_not : term -> Ocaml_common.Warnings.loc -> term

val t_quant :
  quant ->
  vsymbol list ->
  term ->
  ty option ->
  Ocaml_common.Warnings.loc ->
  term

val f_forall :
  vsymbol list -> term -> ty option -> Ocaml_common.Warnings.loc -> term

val f_exists :
  vsymbol list -> term -> ty option -> Ocaml_common.Warnings.loc -> term

val t_lambda :
  vsymbol list -> term -> ty option -> Ocaml_common.Warnings.loc -> term

val f_and : term -> term -> Ocaml_common.Warnings.loc -> term
val f_and_asym : term -> term -> Ocaml_common.Warnings.loc -> term
val f_or : term -> term -> Ocaml_common.Warnings.loc -> term
val f_or_asym : term -> term -> Ocaml_common.Warnings.loc -> term
val f_implies : term -> term -> Ocaml_common.Warnings.loc -> term
val f_iff : term -> term -> Ocaml_common.Warnings.loc -> term
