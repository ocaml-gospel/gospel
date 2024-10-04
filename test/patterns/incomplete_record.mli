(*@ type t = { a : integer; b : integer; c : integer } *)

(* OCaml would report missing fields *)
(*@ function f (x : t) : integer =
      match x with
      | { a } -> a
*)
(* {gospel_expected|
   [125] File "incomplete_record.mli", line 6, characters 8-13:
         6 |       | { a } -> a
                     ^^^^^
         Error: Some record fields are undefined: b c.
   |gospel_expected} *)
