module type S = sig
  val s : string
end

val f : (module S) -> string
(*@ s = f m *)
(* {gospel_expected|
[125] gospel: internal error, uncaught exception:
              File "src/uattr2spec.ml", line 85, characters 9-15: Assertion failed
              
      
|gospel_expected} *)
