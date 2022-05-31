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
    | B (A, x) -> () *)

(* EXPECTED
   [125] File "float.mli", line 4, characters 4-182:
         Error: This pattern-matching is not exhaustive.
                Here is an example of a case that is not matched:
                  B (B (A, 3.), 0.).
*)
