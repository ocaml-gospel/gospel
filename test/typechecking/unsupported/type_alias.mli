type t0 = A | B
type t1 = t0
(*@ model : integer *)
(* {gospel_expected|
[125] gospel: internal error, uncaught exception:
              File "src/uattr2spec.ml", line 102, characters 9-15: Assertion failed
              
      
|gospel_expected} *)
