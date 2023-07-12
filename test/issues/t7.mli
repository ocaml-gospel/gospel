val f : int option -> int
(*@ r = f x
    requires match x with
             | Some (y:int) -> y >= 0
             | None -> true *)

(* {gospel_expected|
   [125] gospel: internal error, uncaught exception:
                 File "src/dterm.ml", line 332, characters 18-24: Assertion failed
                 
   |gospel_expected} *)
