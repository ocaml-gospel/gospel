type t = A | B
(*@ model : integer *)
(* {gospel_expected|
[1] File "./variant_with_spec.mli", line 1, characters 0-37:
    1 | type t = A | B
    2 | (*@ model : integer *)
    Error: Not yet supported: variant type
    
|gospel_expected} *)
