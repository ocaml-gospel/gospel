module type S = sig val x : int end
val s : (module S)
(* {gospel_expected|
   [125] gospel: internal error, uncaught exception:
                 File "src/typing.ml", line 113, characters 9-15: Assertion failed
                 
   |gospel_expected} *)
