type t = [ `A | `B ]
(* {gospel_expected|
   [125] gospel: internal error, uncaught exception:
                 File "src/typing.ml", line 622, characters 11-17: Assertion failed
                 
   |gospel_expected} *)
