type t = A | B

exception E of t

val f : int -> int
(*@ y = f x
    raises E e
      ensures True *)
(* {gospel_expected|
[125] gospel: internal error, uncaught exception:
              File "src/uattr2spec.ml", line 102, characters 9-15: Assertion failed
              
      
|gospel_expected} *)
