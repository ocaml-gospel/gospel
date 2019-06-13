exception E of int list


val f : 'a -> 'a
(*@ x = f y
    raises E l -> match l with
                  | [] -> false
                  | y :: ys -> y = 2 *)

(* ERROR:
   Line 6
   y is of type int and 8 of type integer
   use integer_of_int y *)
