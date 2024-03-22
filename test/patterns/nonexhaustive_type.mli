type t = A1 | A2 | B of t

(*@ function f (x: t): unit =
    match x with
    | A1 | A2
    | B A2 | B A1
    | B (B A1) | B (B A2)
    | B (B (B A1))
    | B (B (B A2)) -> () *)

(* {gospel_expected|
   [125] File "nonexhaustive_type.mli", line 4, characters 4-118:
         4 | ....match x with
         5 |     | A1 | A2
         6 |     | B A2 | B A1
         7 |     | B (B A1) | B (B A2)
         8 |     | B (B (B A1))
         9 |     | B (B (B A2)) -> ()...
         Error: This pattern-matching is not exhaustive.
                Here is an example of a case that is not matched:
                  B B B B _.
   |gospel_expected} *)
