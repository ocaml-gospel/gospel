(*@ type t = { a : bool } *)

(*@ predicate p (x : t) =
  let y = { a = true ; b = 42 } in true
*)

(* {gospel_expected|
   [125] File "value_unbound_record_field.mli", line 4, characters 10-31:
         4 |   let y = { a = true ; b = 42 } in true
                       ^^^^^^^^^^^^^^^^^^^^^
         Error: Unbound record field: b.
   |gospel_expected} *)
