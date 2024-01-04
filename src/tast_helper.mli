open Ppxlib
open Tast
open Tterm
open Symbols
open Ttypes

val ty_of_lb_arg : lb_arg -> ty
val vs_of_lb_arg : lb_arg -> vsymbol

val type_spec :
  bool ->
  (lsymbol * bool) list ->
  (vsymbol * term list) option ->
  string ->
  Location.t ->
  type_spec

val label_declaration :
  'a -> mutable_flag -> Location.t -> attributes -> 'a label_declaration

val mk_val_spec :
  lb_arg list ->
  lb_arg list ->
  term list ->
  term list ->
  term list ->
  (xsymbol * (pattern * term) list) list ->
  term list ->
  term list ->
  bool ->
  bool ->
  string list ->
  string ->
  Location.t ->
  val_spec

val mk_fun_spec :
  term list ->
  term list ->
  term list ->
  bool ->
  string ->
  Location.t ->
  fun_spec

val mk_function :
  ?result:vsymbol ->
  lsymbol ->
  bool ->
  vsymbol list ->
  term option ->
  fun_spec option ->
  Location.t ->
  string ->
  function_

val mk_axiom : Ident.t -> term -> Location.t -> string -> axiom

val mk_val_description :
  Ident.t ->
  core_type ->
  string list ->
  attributes ->
  lb_arg list ->
  lb_arg list ->
  val_spec option ->
  Location.t ->
  val_description

val type_declaration :
  tysymbol ->
  (tvsymbol * (Asttypes.variance * Asttypes.injectivity)) list ->
  (ty * ty * Location.t) list ->
  type_kind ->
  private_flag ->
  ty option ->
  attributes ->
  type_spec option ->
  Location.t ->
  type_declaration

val extension_constructor :
  Ident.t ->
  xsymbol ->
  extension_constructor_kind ->
  Location.t ->
  attributes ->
  extension_constructor

val constructor_decl :
  lsymbol ->
  (Ident.t * ty) label_declaration list ->
  Location.t ->
  attributes ->
  constructor_decl

val type_exception :
  extension_constructor -> Location.t -> attributes -> type_exception

val mk_sig_item : signature_item_desc -> Location.t -> signature_item
