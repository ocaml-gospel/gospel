module type S = sig
  val s : string
end

val f : (module S) -> string
(*@ s = f m *)
(* {gospel_expected|
[125] gospel: internal error, uncaught exception:
              Gospel_checker.Uattr2spec.Unsupported
              
      
|gospel_expected} *)
