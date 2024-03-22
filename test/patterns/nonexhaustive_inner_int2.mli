type t = A | B of int

(*@ function f1 (x: t): unit =
    match x with
    | A -> ()
    | B 1i -> ()
    | B 2i -> () *)

(* {gospel_expected|
   [125] File "nonexhaustive_inner_int2.mli", line 4, characters 4-64:
         4 | ....match x with
         5 |     | A -> ()
         6 |     | B 1i -> ()
         7 |     | B 2i -> ()...
         Error: This pattern-matching is not exhaustive.
                Here is an example of a case that is not matched:
                  B 0i.
   |gospel_expected} *)
