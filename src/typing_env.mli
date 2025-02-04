module W = Warnings
open Ppxlib
open Dterm
open Ttypes
open Uast
open Tast
open Tmodule
open Symbols
open Identifier

type 'a env
(** The typing environment. Keeps track of:

    - [whereami]
    - the global environment, that is information that has been defined
      previously in the module or in another module
    - the local typing environment parameterized over 'a, that is information
      about variables in the local scope of the term beeing typed
    - coercions
    - [namespace] *)

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

exception Ns_not_found of location * string

val make_new : signature_item Mid.t -> Coercion.t -> namespace -> 'a env
(** [make_new global coercions namespace] builds a new typing environment with
    the given informations. the [whereami] information is not set and the local
    typing environment is empyt. *)

val env_of_module_uc : module_uc -> 'a env
(** [env_of_module_uc muc] is [make_new global coercions namespace] where the
    three arguments come from [muc]. *)

(** Get part of the environment. *)

val get_coercions : 'a env -> Coercion.t
val get_namespace : 'a env -> namespace
val get_local : 'a env -> 'a Mstr.t

(** Lookup the environment. *)

val whereami : 'a env -> whereami
(** [whereami env] returns [whereami].

    @raise Invalid_argument if the information is not set. *)

val mem_local : 'a env -> string -> bool
(** [mem_local env s] is true iff [s] is mapped to an 'a in the local typing
    environment. *)

val find_local_opt : 'a env -> string -> 'a option
(** [find_local_opt env s] is the optional 'a mapped to [s] in the local typing
    environment. *)

val find_local_exn : loc:Location.t -> 'a env -> string -> 'a
(** [find_local_exn env s] is the 'a mapped to [s] in the local typing
    environment.

    @raise Unbound_variable if there is no mapping. *)

val find_ls : loc:Location.t -> 'a env -> string list -> lsymbol
(** [find_ls loc env path] returns the logical symbol representing a data
    constructor or a function mapped to [path] in the current namespace.

    @raise Symbol_not_found *)

val find_ts : loc:Location.t -> 'a env -> string list -> tysymbol
(** [find_ts loc env path] returns the type symbol mapped to [path] in the
    current namespace.

    @raise Symbol_not_found *)

val find_xs : loc:Location.t -> 'a env -> string list -> xsymbol
(** [find_xs loc env path] returns the exception symbol mapped to [path] in the
    current namespace.

    @raise Symbol_not_found *)

val find_q_ls : 'a env -> qualid -> lsymbol
(** [find_q_ls loc env qualid] returns the logical symbol representing a data
    constructor or a function mapped to [qualid] in the current namespace.

    @raise Symbol_not_found *)

val find_q_ts : 'a env -> qualid -> tysymbol
(** [find_q_ts loc env qualid] returns the type symbol mapped to [qualid] in the
    current namespace.

    @raise Symbol_not_found *)

val find_q_fd : 'a env -> qualid -> lsymbol
(** [find_q_fd env qualid] returns the logical symbol representing a record's
    field or a logical model mapped to [qualid].

    @raise Symbol_not_found *)

val find_q_xs : 'a env -> qualid -> xsymbol
(** [find_q_xs loc env qualid] returns the exception symbol mapped to [qualid]
    in the current namespace.

    @raise Symbol_not_found *)

val find_q_ns : 'a env -> qualid -> namespace
(** [find_q_ns env qualid] returns the namespace mapped to [qualid]. The
    namespace corresponds to a module declaration.

    @raise Symbol_not_found *)

val find_constructors : 'a env -> tysymbol -> lsymbol * lsymbol list
(** [find_constructors env ts] returns the constructors and the fields symbols
    corresponding to [ts] if it is the symbol for a record type. This function
    is not safe to use with symbol for non-record types. *)

(** Modify typing environment *)

val set_namespace : 'a env -> namespace -> 'a env
val set_whereami : 'a env -> whereami -> 'a env
val union_local : 'a env -> 'a Mstr.t -> 'a env

val add_local_var_no_duplicate : vsymbol env -> string -> vsymbol -> vsymbol env
(** [add_local_var_no_duplicate env s x] adds a mapping from [s] to [x] in the
    local typing environment. Doesn't allow for shadowing previous mapping.

    @raise Duplicated_variable *)

val add_local_var : 'a env -> string -> 'a -> 'a env
(** [add_local_var env s x] adds a mapping from [s] to [x] in the local typing
    environment. Allows for shadowing previous mapping. *)

val add_local_vars : 'a env -> (Preid.t * 'a) list -> 'a env
(** [add_local_vars env xs] adds all the mapping from [xs], granted that it
    doesn't contains any duplicates. Allows for shadowing previous mapping.

    @raise Duplicated_variable *)

val add_ns_fd : 'a env -> string -> lsymbol -> 'a env
(** [add_ns_fs env s ls] adds the mapping from [s] to [ls] to the record fields
    and logical models namespace. *)

(** Utils *)

val dty_env_of_vsymbol_env : vsymbol env -> dty env
val string_list_of_qualid : Uast.qualid -> string list
