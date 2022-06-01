type a = A of string | B

(*@ function f (x: a): unit =
    match x with
    | B
    | A ""
    | A "1"
    | A "22"
    | A "333"
    | A "4444"
    | A "55555"
    | A "666666" -> () *)

(* {gospel_expected|
   [125] File "str.mli", line 4, characters 4-128:
         Error: This pattern-matching is not exhaustive.
                Here is an example of a case that is not matched:
                  A "???????".
   |gospel_expected} *)
