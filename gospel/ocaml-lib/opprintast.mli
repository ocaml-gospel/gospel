(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Hongbo Zhang (University of Pennsylvania)                  *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)


(** Pretty-printers for {!Oparsetree}

  {b Warning:} this module is unstable and part of
  {{!Compiler_libs}compiler-libs}.

*)

type ctxt

type space_formatter = (unit, Format.formatter, unit) format

val pp : Format.formatter -> ('a, Format.formatter, unit) format -> 'a

val list :  ?sep:space_formatter -> ?first:space_formatter ->
  ?last:space_formatter -> (Format.formatter -> 'a -> unit) ->
  Format.formatter -> 'a list -> unit

val reset_ctxt : ctxt
val constant_string : Format.formatter -> string -> unit

val nonrec_flag : Format.formatter -> Oasttypes.rec_flag -> unit
val type_params : ctxt -> Format.formatter -> 
                  (Oparsetree.core_type * Oasttypes.variance) list -> unit
val constructor_declaration : ctxt -> Format.formatter ->
                              string * Oparsetree.constructor_arguments
                                     * Oparsetree.core_type option
                                     * Oparsetree.attributes
                              -> unit
val record_declaration : ctxt -> Format.formatter 
                         -> Oparsetree.label_declaration list -> unit
val attributes : ctxt -> Format.formatter -> Oparsetree.attributes -> unit
val protect_ident : Format.formatter -> string -> unit
val type_extension : ctxt -> Format.formatter -> Oparsetree.type_extension -> unit
val exception_declaration : ctxt -> Format.formatter 
                            -> Oparsetree.type_exception -> unit
val virtual_flag : Format.formatter -> Oasttypes.virtual_flag -> unit
val class_params_def : ctxt -> Format.formatter -> 
                       (Oparsetree.core_type * Oasttypes.variance) list -> unit
val class_type : ctxt -> Format.formatter -> Oparsetree.class_type -> unit
val longident_loc : Format.formatter -> Longident.t Location.loc -> unit
val module_type : ctxt -> Format.formatter -> Oparsetree.module_type -> unit
val module_expr : ctxt -> Stdlib__format.formatter -> Oparsetree.module_expr -> unit
val extension : ctxt -> Format.formatter -> Oparsetree.extension -> unit
val class_type_declaration_list : ctxt -> Format.formatter -> 
                                  Oparsetree.class_type Oparsetree.class_infos list 
                                  -> unit
val module_type1 : ctxt -> Format.formatter -> Oparsetree.module_type -> unit
val floating_attribute : ctxt -> Format.formatter -> Oparsetree.attribute -> unit
val item_attributes : ctxt -> Format.formatter -> Oparsetree.attributes -> unit
val item_extension : ctxt -> Format.formatter -> 
                     string Oasttypes.loc * Oparsetree.payload -> unit
val override : Oasttypes.override_flag -> string
val paren : ?first:space_formatter -> ?last:space_formatter ->
            bool -> (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a -> unit


val longident : Format.formatter -> Longident.t -> unit
val expression : Format.formatter -> Oparsetree.expression -> unit
val string_of_expression : Oparsetree.expression -> string
val constant : Format.formatter -> Oasttypes.constant -> unit

val pattern: Format.formatter -> Oparsetree.pattern -> unit

val core_type: Format.formatter -> Oparsetree.core_type -> unit

val signature: Format.formatter -> Oparsetree.signature -> unit
val structure: Format.formatter -> Oparsetree.structure -> unit
val string_of_structure: Oparsetree.structure -> string

val toplevel_phrase : Format.formatter -> Oparsetree.toplevel_phrase -> unit
val top_phrase: Format.formatter -> Oparsetree.toplevel_phrase -> unit
