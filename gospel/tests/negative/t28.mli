(*@ axiom ax: forall x: float list.
    match x with
    | y :: ys -> y = 2 *)

(* ERROR:
   Line 3
   y is of type float and 2 of type integer
   replace "2" by "2." in line 3 *)
