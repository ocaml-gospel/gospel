type t = A | B of int

(*@ function f1 (x: t): unit =
    match x with
    | A -> ()
    | B 1i -> ()
    | B 2i -> () *)

(* EXPECTED
   [125] File "int.mli", line 4, characters 4-64:
         Error: This pattern-matching is not exhaustive.
                Here is an example of a case that is not matched:
                  B 0i.
*)
