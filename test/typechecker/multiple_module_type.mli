module type S = sig end
module type S = sig end
(* {gospel_expected|
   [125] File "multiple_module_type.mli", line 2, characters 0-23:
         2 | module type S = sig end
             ^^^^^^^^^^^^^^^^^^^^^^^
         Error: Multiple definition of the module type name S.
                Names must be unique in a given signature.
   |gospel_expected} *)
