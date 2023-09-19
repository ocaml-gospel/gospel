type 'a t
(*@ mutable model contents: 'a list *)

val f : 'a t -> unit
(*@ f xs
    modifies xs
    ensures xs = { contents = [] }
*)

(* {gospel_expected|
   [125] gospel: internal error, uncaught exception:
                 File "src/typing.ml", line 124, characters 13-19: Assertion failed
                 
   |gospel_expected} *)
