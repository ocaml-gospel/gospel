open Ppxlib
open Ttypes
open Tterm

val check_exhaustive : loc:location -> ty -> (pattern * term option * term) list -> unit
