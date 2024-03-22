val f : int -> int
(*@ y = f x *)

val g : int -> int
(*@ y = g x
    requires f x > 0
*)

(* An impure OCaml function is not promoted to a Gospel function *)

(* {gospel_expected|
   [125] File "impure_not_promoted.mli", line 6, characters 13-14:
         6 |     requires f x > 0
                          ^
         Error: Symbol f not found in scope
                (see "Symbols in scope" documentation page).
   |gospel_expected} *)
