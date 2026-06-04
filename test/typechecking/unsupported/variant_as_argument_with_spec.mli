type t = A | B

val f : t -> bool
(*@ b = f x *)
(* {gospel_expected|
[125] gospel: internal error, uncaught exception:
              File "src/uattr2spec.ml", line 102, characters 9-15: Assertion failed
              
      
|gospel_expected} *)
