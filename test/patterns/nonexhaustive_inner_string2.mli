type t = { n : int; s : string }

(*@ function f (x: t): unit =
    match x with
    | { n=p; s=("alpha"|"beta"|"gamma") } -> ()
    | { n=p; s="" } -> () *)

(* {gospel_expected|
   [125] File "nonexhaustive_inner_string2.mli", line 4, characters 4-90:
         4 | ....match x with
         5 |     | { n=p; s=("alpha"|"beta"|"gamma") } -> ()
         6 |     | { n=p; s="" } -> ()...
         Error: This pattern-matching is not exhaustive.
                Here is an example of a case that is not matched:
                  constr#t (0i, "?").
   |gospel_expected} *)
