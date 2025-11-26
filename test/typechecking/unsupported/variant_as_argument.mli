type t = A | B

val f : t -> bool
(* {gospel_expected|
[1] File "./variant_as_argument.mli", line 3, characters 8-9:
    3 | val f : t -> bool
                ^
    Error: Unbound type constructor t
    
|gospel_expected} *)
