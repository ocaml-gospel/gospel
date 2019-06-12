(*@ axiom ax: forall x: int list.
    match x with
    | y :: ys -> y = 2 *)

(* ERROR:
   Line 3
   y is of type int and 2 of type integer
   use integer_of_int y *)
