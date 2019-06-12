val f : 'a -> 'a
(*@ x = f y
    raises E1 x -> integer_of_int x = 1
    raises E3 l -> match l with
                   | [] -> false
                   | y :: ys -> y = 2 *)

(* ERROR:
   Line 6
   y is of type int and 2 of type integer
   use integer_of_int y *)
