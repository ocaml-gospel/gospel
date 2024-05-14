module type S = sig val x : int end
val s : (module S)
(* {gospel_expected|
   [125] File "first_class_module.mli", line 2, characters 8-18:
         2 | val s : (module S)
                     ^^^^^^^^^^
         Error: Not yet supported: first class module.
   |gospel_expected} *)
