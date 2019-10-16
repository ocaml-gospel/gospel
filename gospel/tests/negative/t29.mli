exception E of float list


val f : 'a -> 'a
(*@ x = f y
    raises E l -> match l with
                  | [] -> false
                  | y :: ys -> y = 2 *)

(* ERROR:
   Line 6
   y is of type float and 8 of type integer
   replace "2" by "2." in line 8 *)
