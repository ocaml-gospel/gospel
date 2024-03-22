type t = { a : int }

val f : t -> int
(*@ r = f x
      ensures r = a x
*)

(* {gospel_expected|
   [125] File "field_application.mli", line 5, characters 18-19:
         5 |       ensures r = a x
                               ^
         Error: Symbol a not found in scope
                (see "Symbols in scope" documentation page).
   |gospel_expected} *)
