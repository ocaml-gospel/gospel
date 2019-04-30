val f : x:('a -> 'b -> 'c) -> y:'a -> ('b -> 'c)
(*@ r = f ~x [z:int] ~y z *)

(* ERROR:
   Line 2
   duplicated names in function header
   change name of z in line 2 *)
