open Tast
module Mid : Map.S with type key = Ident.t
module Mstr : Map.S with type key = string

type namespace
type known_ids = signature_item Mid.t
type file = { fl_nm : Ident.t; fl_sigs : signature; fl_export : namespace }

type module_uc = {
  muc_nm : Ident.t;
  muc_file : string;
  muc_sigs : signature list;
  muc_prefix : string list;
  (* essential when closing namespaces *)
  muc_import : namespace list;
  muc_export : namespace list;
  muc_files : file Mstr.t;
  muc_kid : known_ids;
  muc_crcm : Coercion.t;
}

val empty_ns : namespace
val type_declarations : type_declaration Ttypes.Hts.t
val ns_exists_ns : namespace -> string -> bool
val ns_exists_tns : namespace -> string -> bool
val ns_find_ts : namespace -> string list -> Ttypes.tysymbol
val ns_find_ls : namespace -> string list -> Symbols.lsymbol
val ns_find_fd : namespace -> string list -> Symbols.lsymbol
val ns_find_xs : namespace -> string list -> Ttypes.xsymbol
val ns_find_ns : namespace -> string list -> namespace
val ns_find_tns : namespace -> string list -> namespace

val ns_add_ls :
  allow_duplicate:bool -> namespace -> string -> Symbols.lsymbol -> namespace

val ns_add_fd :
  allow_duplicate:bool -> namespace -> string -> Symbols.lsymbol -> namespace

val add_ns : ?export:bool -> module_uc -> string -> namespace -> module_uc
val add_ns_top : ?export:bool -> module_uc -> namespace -> module_uc
val init_muc : string -> module_uc
val open_empty_module : module_uc -> string -> module_uc
val close_module_file : module_uc -> module_uc
val open_module : module_uc -> string -> module_uc
val close_module_functor : module_uc -> module_uc
val close_module : module_uc -> module_uc
val close_module_type : module_uc -> module_uc
val add_sig_contents : module_uc -> Tast.signature_item -> module_uc
val get_top_import : module_uc -> namespace
val get_top_sigs : module_uc -> signature
val muc_replace_ts : module_uc -> Ttypes.tysymbol -> string list -> module_uc
val muc_rm_ts : module_uc -> string list -> module_uc
val muc_subst_ts : module_uc -> Ttypes.tysymbol -> Ttypes.tysymbol -> module_uc

val muc_subst_ty :
  module_uc -> Ttypes.tysymbol -> Ttypes.tysymbol -> Ttypes.ty -> module_uc

val get_file : module_uc -> string -> file
val wrap_up_muc : module_uc -> file
val read_gospel_file : string -> module_uc
val path2module : string -> string
val print_file : file Fmt.t
val write_gospel_file : module_uc -> unit
