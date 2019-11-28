(********************************************************************)
(*                                                                  *)
(*  The Why3 Verification Platform   /   The Why3 Development Team  *)
(*  Copyright 2010-2019   --   Inria - CNRS - Paris-Sud University  *)
(*                                                                  *)
(*  This software is distributed under the terms of the GNU Lesser  *)
(*  General Public License version 2.1, with the special exception  *)
(*  on linking described in file LICENSE.                           *)
(*                                                                  *)
(********************************************************************)

exception Ocaml_syntax_error of Location.t
exception FileNotFound of string

(** `parse_ocaml load_path file` parses the OCaml content of the
   `file` if it is a valid interface. If `file` is relative, it
   searches for `file` in the `load_path`.

   Raise FileNotFound if file does not exist.

   Raise Ocaml_syntax_error if there is an OCaml syntax error. *)
val parse_ocaml : string list -> string -> Oparsetree.signature

(** `parse_gospel sig_list name` parses the GOSPEL attributes and
   integrates them in the corresponding OCaml signatures. *)
val parse_gospel :
  Oparsetree.signature_item list -> string -> Uast.s_signature_item list

(** `parse_ocaml_gospel load_path file` parses the OCaml interface and
   the GOSPEL specification of `file`. If `file` is relative, it
   searches for `file` in the `load_path`.

   Raise FileNotFound if file does not exist.

   Raise Ocaml_syntax_error if there is an OCaml syntax error. *)
val parse_ocaml_gospel :
  string list -> string -> string -> Uast.s_signature_item list
