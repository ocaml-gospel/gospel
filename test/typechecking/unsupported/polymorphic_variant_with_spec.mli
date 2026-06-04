type t = [ `A | `B ]
(*@ model : integer *)
(* {gospel_expected|
[125] gospel: internal error, uncaught exception:
              File "src/uattr2spec.ml", line 85, characters 9-15: Assertion failed
              
      
|gospel_expected} *)
