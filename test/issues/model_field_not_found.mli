type 'a t = 'a list
(*@ mutable model contents: 'a list *)

val f : 'a t -> unit
(*@ f xs
    modifies xs
    ensures xs = { contents = [] }
*)

(* The following error was tracked down to src/typing.ml:107 *)
(* {gospel_expected|
   [125] gospel: internal error, uncaught exception:
                 Not_found
                 
   |gospel_expected} *)
