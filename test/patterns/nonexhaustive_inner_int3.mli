type a = A of int | B

(*@ function f (x: a): unit =
    match x with
    | B
    | A 0i
    | A 1i
    | A 2i
    | A 3i
    | A 5i
    | A 6i -> () *)

(* {gospel_expected|
   [125] File "nonexhaustive_inner_int3.mli", line 4, characters 4-96:
          4 | ....match x with
          5 |     | B
          6 |     | A 0i
          7 |     | A 1i
          8 |     | A 2i
          9 |     | A 3i
         10 |     | A 5i
         11 |     | A 6i -> ()...
         Error: This pattern-matching is not exhaustive.
                Here is an example of a case that is not matched:
                  A 4i.
   |gospel_expected} *)
