module M : sig end
module M : sig end
(* {gospel_expected|
   [125] File "multiple_module_definition.mli", line 2, characters 0-18:
         2 | module M : sig end
             ^^^^^^^^^^^^^^^^^^
         Error: Multiple definition of the module name M.
                Names must be unique in a given signature.
   |gospel_expected} *)
