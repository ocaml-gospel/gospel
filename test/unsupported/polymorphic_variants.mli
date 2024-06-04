type t = [ `A | `B ]
(* {gospel_expected|
   [125] File "polymorphic_variants.mli", line 1, characters 9-20:
         1 | type t = [ `A | `B ]
                      ^^^^^^^^^^^
         Error: Not yet supported: polymorphic variant.
   |gospel_expected} *)
