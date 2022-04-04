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
  term list ->
  string ->
  Location.t ->
  type_spec

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
