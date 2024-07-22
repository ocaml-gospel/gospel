(*@ type t = { a : bool } *)

(*@ predicate p (x : t) =
  let y = { a = true ; b = 42 } in true
*)

(* {gospel_expected|
   [125] File "value_unbound_record_field.mli", line 4, characters 23-24:
         4 |   let y = { a = true ; b = 42 } in true
                                    ^
         Error: Symbol b not found in scope
                (see "Symbols in scope" documentation page).
   |gospel_expected} *)
