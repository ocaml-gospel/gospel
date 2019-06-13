exception E of int * int

(*@ function integer_of_int (x:int): integer *)
(*@ function fst (x: 'a * 'a): 'a *)

val f : 'a -> 'a
(*@ x = f y
    raises E x -> integer_of_int (fst x) = 1 *)

(* ERROR:
   Line 8
   exception E has two arguments
   change pattern in line 8, or define E, as exception E of (int * int) *)
