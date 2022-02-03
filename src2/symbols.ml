open Identifier
open Common

(* value symbol *)
type vsymbol = { vs_name : Ident.t; vs_ty : Ttypes.ty; vs_ghost : ghost }

(* function and predicate symbol *)
type lsymbol = {
  ls_name : Ident.t;
  ls_ty : Ttypes.ty; (* A lsymbol is always a ghost value *)
}

(* model/field symbol, (should we call it fsymbol ?) *)
type msymbol = { ms_name : Ident.t; ms_ty : Ttypes.ty; ms_ghost : ghost }

(* construct symbol *)
type csymbol = { cs_name : Ident.t; cs_ty : Ttypes.ty; cs_ghost : ghost }

(* exception symbol *)
type xsymbol = { xs_name : Identifier.Ident.t; xs_type : Ttypes.exn_type }
