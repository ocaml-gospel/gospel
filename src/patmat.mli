open Ppxlib
open Ttypes
open Tterm

val checks : loc:location -> ty -> (pattern * term option * term) list -> unit
