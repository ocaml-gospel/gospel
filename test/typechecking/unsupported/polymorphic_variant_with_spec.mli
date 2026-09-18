type t = [ `A | `B ]
(*@ model : integer *)
(* {gospel_expected|
[1] File "./polymorphic_variant_with_spec.mli", line 1, characters 9-20:
    1 | type t = [ `A | `B ]
                 ^^^^^^^^^^^
    Error: Not yet supported: polymorphic variant
    
|gospel_expected} *)
