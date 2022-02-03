open Ppxlib

(* come frome Parsetree, so that we don't depend on it, at least for devel time *)
type constant =
  | Pconst_integer of string * char option
  (* 3 3l 3L 3n

     Suffixes [g-z][G-Z] are accepted by the parser.
     Suffixes except 'l', 'L' and 'n' are rejected by the typechecker
  *)
  | Pconst_char of char
  (* 'c' *)
  | Pconst_string of string * Location.t * string option
  (* "constant"
     {delim|other constant|delim}

     The location span the content of the string, without the delimiters.
  *)
  | Pconst_float of string * char option
(*IF_CURRENT = Parsetree.constant *)
(* 3.4 2e5 1.4e-4

   Suffixes [g-z][G-Z] are accepted by the parser.
   Suffixes are rejected by the typechecker.
*)

type pattern = {
  p_node : pattern_node;
  p_ty : Ttypes.ty;
  (* what for? At least, this could be computed *)
  p_vars : Symbols.vsymbol list;
  p_loc : Location.t;
}

and pattern_node =
  | Pwild
  | Pvar of Symbols.vsymbol
  | Pconstr of Symbols.csymbol * pattern list
  | Prec of (Symbols.msymbol * pattern) list
  | Ptuple of pattern list
  | Pas of pattern * Symbols.vsymbol
  | Por of pattern * pattern

type quant = Tforall | Texists

type term = {
  t_node : term_node;
  t_ty : Ttypes.ty;
  t_attrs : string list;
  t_loc : Location.t;
}

and terms = term list
and case = { pattern : pattern; term : term }
and cases = case list

and term_node =
  | Tvar of Symbols.vsymbol (* variables *)
  | Tconst of constant (* constants *)
  | Tlet of Symbols.vsymbol * term * term (* let binding *)
  | Tcase of term * cases (* pattern matching *)
  | Tfield of term * Symbols.msymbol (* record/model destructior *)
  | Ttuple of term list (* tuple constructor *)
  | Tif of term * term * term (* conditional construction *)
  | Tquant of quant * Symbols.vsymbol list * term (* quantifiers *)
  | Tfun of Symbols.lsymbol list * term (* anonymous function definition *)
  | Tapp of term * term list (* function application *)
  | Told of term
(* old construction - specific to gospel *)
