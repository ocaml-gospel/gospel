type t = A | B

val f : t -> bool
(*@ b = f x *)
(* {gospel_expected|
[1] File "./variant_as_argument_with_spec.mli", line 3, characters 8-9:
    3 | val f : t -> bool
                ^
    Error: Not yet supported: t
    
|gospel_expected} *)
