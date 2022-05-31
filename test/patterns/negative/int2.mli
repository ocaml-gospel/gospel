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

(* EXPECTED
   [125] File "int2.mli", line 4, characters 4-97:
         Error: This pattern-matching is not exhaustive.
                Here is an example of a case that is not matched:
                  A 4i.
*)
