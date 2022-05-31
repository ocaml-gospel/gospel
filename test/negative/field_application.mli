type t = { a : int }

val f : t -> int
(*@ r = f x
      ensures r = a x *)

(* EXPECTED
   [125] File "field_application.mli", line 5, characters 18-19:
         Error: Symbol a not found.
*)
