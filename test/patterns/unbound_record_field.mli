(*@ type t = { a : bool } *)

(*@ predicate p (x : t) =
  match x with
  | { a; b } -> true
*)

(* {gospel_expected|
   [125] File "unbound_record_field.mli", line 5, characters 9-10:
         5 |   | { a; b } -> true
                      ^
         Error: Symbol b not found in scope
                (see "Symbols in scope" documentation page).
   |gospel_expected} *)
