val f : [ `A | `B ] -> bool
(*@ b = f v *)
(* {gospel_expected|
[1] File "./value_polymorphic_variant_with_spec.mli", line 1, characters 8-19:
    1 | val f : [ `A | `B ] -> bool
                ^^^^^^^^^^^
    Error: Not yet supported: polymorphic variant
    
|gospel_expected} *)
