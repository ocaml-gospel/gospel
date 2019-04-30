val f : x:('a -> 'b -> 'c) -> y:'a -> ('b -> 'c)
(*@ [b:integer],[a:'a] = f ~x [w:int] ~y [p:integer] z *)

(* ERROR:
   Line 2
   no return value in function specification header
   add a new return var in line 2 *)
