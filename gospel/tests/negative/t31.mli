exception E of (int * int)

(*@ function integer_of_int (x:int): integer *)
(*@ function fst (x: 'a * 'a): 'a *)

val f : 'a -> 'a
(*@ x = f y
    raises E (x,y,z) -> integer_of_int x = 1 *)

(* ERROR:
   Line 8
   Pattern for exception E does not match type
   remove one of the tuple elements in line 8 *)
