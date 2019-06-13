exception E of (int * int * int)

(*@ function integer_of_int (x:int): integer *)
(*@ function fst (x: 'a * 'a): 'a *)

val f : 'a -> 'a
(*@ x = f y
    raises E (x,y) -> integer_of_int x = 1 *)

(* ERROR:
   Line 8
   Pattern for exception E does not match type
   add an element to the tuple pattern in line 8 *)
