open Common

type tsymbol = { ts_name : Identifier.Ident.t }

(* version gospel 1 (comes from Why3)
   type tysymbol = {
     ts_ident : Identifier.t;
     (* these two last are weird *)
     (* it seems we never use ts_args, or at least we could compute it *)
     ts_args : tvsymbol list;
     (* OCaml core language has a Ptyp_alias for that one *)
     ts_alias : ty option;
   }
*)

(* OCaml version (Typedtree)
   type core_type_desc =
   |	Ptyp_any
   |	Ptyp_var of string
   |	Ptyp_arrow of Asttypes.arg_label * core_type * core_type
   |	Ptyp_tuple of core_type list
   |	Ptyp_constr of Longident.t Asttypes.loc * core_type list
   |	Ptyp_object of object_field list * Asttypes.closed_flag
   |	Ptyp_class of Longident.t Asttypes.loc * core_type list
   |	Ptyp_alias of core_type * string
   |	Ptyp_variant of row_field list * Asttypes.closed_flag * Asttypes.label list option
   |	Ptyp_poly of string Asttypes.loc list * core_type
   |	Ptyp_package of package_type
   |	Ptyp_extension of extension
*)

(* XXX do we add kinds ? or at least quantification over types ? *)
type ty =
  | Tyvar of tsymbol
  (* alpha types *)
  | Tyarr of arg_label * ty * ty
  (* unary function type. do we want n-ary functions ? *)
  | Tyapp of tsymbol * ty list
  (* this is equivalent to Ptyp_constr.
     We need a symbol that allows to identify the type constructor *)
  | Tytuple of ty list
  | Tyalias of ty * string
(* this is different from version 1 (coming from Why3).
   we keep the information that a type is an alias at the level of the type representation,
   not the symbol *)

type exn_type =
  | Exn_tuple of ty list
  | Exn_record of (Identifier.Ident.t * ty) list
