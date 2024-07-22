(*@ type t = { a : integer; b : integer; c : integer } *)

(*@ axiom x : let a = { a = 42 } in true *)
(* {gospel_expected|
   [125] File "value_incomplete_record.mli", line 3, characters 22-32:
         3 | (*@ axiom x : let a = { a = 42 } in true *)
                                   ^^^^^^^^^^
         Error: The field b is unknown.
   |gospel_expected} *)
