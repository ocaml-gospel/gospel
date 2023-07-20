type t = A | B of t * float

(*@ function f1 (x: t): unit =
    match x with
    | A -> ()
    | B (B (B _, _), _)
    | B (B (A, 0.1), 0.)
    | B (B (A, 1.0), 0.)
    | B (B (A, 2.3), 1.)
    | B (A, 1.)
    | B (A, 3.)
    | B (A, _) -> () *)

(* {gospel_expected|
   [125] File "nonexhaustive_float.mli", line 4, characters 4-182:
          4 | ....match x with
          5 |     | A -> ()
          6 |     | B (B (B _, _), _)
          7 |     | B (B (A, 0.1), 0.)
          8 |     | B (B (A, 1.0), 0.)
          9 |     | B (B (A, 2.3), 1.)
         10 |     | B (A, 1.)
         11 |     | B (A, 3.)
         12 |     | B (A, _) -> ()...
         Error: This pattern-matching is not exhaustive.
                Here is an example of a case that is not matched:
                  B (B (A, 3.), 0.).
   |gospel_expected} *)
