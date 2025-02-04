open Tast
open Identifier

type namespace
type known_ids = signature_item Mid.t
type file
type module_uc

val empty_ns : namespace
(** The empty namespace *)

val type_declarations : type_declaration Ttypes.Hts.t
val ns_exists_ns : namespace -> string -> bool
val ns_exists_tns : namespace -> string -> bool

val ns_find_ts : namespace -> string list -> Ttypes.tysymbol
(** [ns_find_ts ns path] find the type symbol mapped to [path].

    @raise Not_found *)

val ns_find_ls : namespace -> string list -> Symbols.lsymbol
(** [ns_find_ls ns path] find the logical symbol representing a data constructor
    or a function mapped to [path].

    @raise Not_found *)

val ns_find_fd : namespace -> string list -> Symbols.lsymbol
(** [ns_find_fd ns path] find the logical symbol representing a record's field
    or a logical model mapped to [path].

    @raise Not_found *)

val ns_find_xs : namespace -> string list -> Ttypes.xsymbol
(** [ns_find_xs ns path] find the exception symbol mapped to [path].

    @raise Not_found *)

val ns_find_ns : namespace -> string list -> namespace
(** [ns_find_ns ns path] find the namespace mapped to [path]. The namespace
    corresponds to a module declaration.

    @raise Not_found *)

val ns_find_tns : namespace -> string list -> namespace
(** [ns_find_tns ns path] find the namespace mapped to [path]. The namespace
    corresponds to a module type definition.

    @raise Not_found *)

val ns_add_ls :
  allow_duplicate:bool -> namespace -> string -> Symbols.lsymbol -> namespace
(** [ns_add_ls ~allow_duplicate ns id ls] adds a mapping from [id] to [ls] in
    [nm]. This is used both for data constructors and functions. Note that
    [allow_duplicate] is overrided to [true] allowing name shadowing for data
    constructors and functions.*)

val ns_add_fd :
  allow_duplicate:bool -> namespace -> string -> Symbols.lsymbol -> namespace
(** [ns_add_fd ~allow_duplicate ns id ls] adds a mapping from [id] to [ls] in
    [nm]. This is used for record's fields and logical models. Note that
    [allow_duplicate] is overrided to [true] allowing name shadowing for records
    fields and logical models. *)

val add_ns : ?export:bool -> module_uc -> string -> namespace -> module_uc
(** [add_ns ~export muc id ns] adds a mapping from [id] to [ns] in the import
    list of [muc]. If [export] then it also adds the mapping to the export list.
    Default value for [export] is [false].*)

val add_ns_top : ?export:bool -> module_uc -> namespace -> module_uc
(** [add_ns_top ~export muc ns] adds all the mapping from [ns] to the import
    list of [muc]. If [export] then it also adds the mappings to the export
    list. Default value for [export] is [false].*)

val init_muc : string -> module_uc
(** [init_muc name] creates a new module under construction containing only
    built-ins elements. *)

val open_module : module_uc -> string -> module_uc
(** [open_module muc name] opens a sub-module [name] in [muc]. *)

val open_empty_module : module_uc -> string -> module_uc
(** [open_empty_module muc name] opens a new empty module [name] to be filled in
    [muc]. *)

val close_module : module_uc -> module_uc
(** [close_module muc] closes the last opened module in [muc]. Adds the
    namespace correponding to the closed module declaration to the import and
    export lists. *)

val close_module_functor : module_uc -> module_uc
(** [close_module_functor muc] closes the last opened module in [muc]. Intended
    to be used for functor arguments. Adds the namespace corresponding to the
    closed module to the import list. *)

val close_module_type : module_uc -> module_uc
(** [close_module_type muc] closes the last opened module in [muc]. Adds the
    namespace correponding to the closed module type to the import and export
    lists. *)

val close_module_file : module_uc -> module_uc
(** [close_module_file muc] closes the last opened module in [muc] and add it as
    a [file]. Adds the namespace corresponding to the closed module to the
    import list. *)

val add_sig_contents : module_uc -> Tast.signature_item -> module_uc
(** [add_sig_contents muc item] adds [item] to [muc] *)

val get_top_import : module_uc -> namespace
val get_top_sigs : module_uc -> signature
val get_module_name : module_uc -> Ident.t
val get_known_ids : module_uc -> known_ids
val get_coercions : module_uc -> Coercion.t

val muc_replace_ts : module_uc -> Ttypes.tysymbol -> string list -> module_uc
(** [muc_replace_ts muc new_ts path] replaces the [tysymbol] mapped to [path]
    with [new_ts] in the tip of the import and export lists. *)

val muc_rm_ts : module_uc -> string list -> module_uc
(** [muc_rm_ts muc path] removes the mapping from [path] to a type symbol in the
    top of the import and export lists. *)

val muc_subst_ts : module_uc -> Ttypes.tysymbol -> Ttypes.tysymbol -> module_uc
(** [muc_subst_ts old_ts new_ts] substitutes all the occurences of [old_ts] with
    [new_ts] in the top of the import and export lists. *)

val muc_subst_ty :
  module_uc -> Ttypes.tysymbol -> Ttypes.tysymbol -> Ttypes.ty -> module_uc

val get_file_export : file -> namespace
val get_file_signature : file -> Tast.signature
val get_file : module_uc -> string -> file
val wrap_up_muc : module_uc -> file
val read_gospel_file : string -> module_uc
val path2module : string -> string
val print_file : file Fmt.t
val write_gospel_file : module_uc -> unit
