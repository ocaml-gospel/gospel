(*@ type t = { a : bool } *)

(*@ predicate p (x : t) =
  match x with
  | { a; b } -> true
*)

(* {gospel_expected|
   [125] File "unbound_record_field.mli", line 5, characters 4-12:
         5 |   | { a; b } -> true
                 ^^^^^^^^
         Error: Unbound record field: b.
   |gospel_expected} *)
