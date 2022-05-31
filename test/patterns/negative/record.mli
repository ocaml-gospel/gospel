type t = { n : int; s : string }

(*@ function f (x: t): unit =
    match x with
    | { n=p; s=("alpha"|"beta"|"gamma") } -> ()
    | { n=p; s="" } -> () *)

(* EXPECTED
   [125] File "record.mli", line 4, characters 4-90:
         Error: This pattern-matching is not exhaustive.
                Here is an example of a case that is not matched:
                  constr#t (0i, "?").
*)
