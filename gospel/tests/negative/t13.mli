
(*@ function int_of_integer (x:integer): int *)
(*@ function to_integer (i: int): integer *)

(*@ function i (a:int):int =
      int_of_integer (to_integer a + 1)
    requires to_integer a > 0
    ensures let old_a [@ athing] = (old a) in
            to_integer a = old_a + 1*)

(* ERROR:
   Line 9
   type mysmatch int with integer
   replace line 13 "ensures let old_a [@ athing] = to_integer (old a) in"
 *)
