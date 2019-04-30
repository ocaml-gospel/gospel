val f : x:('a -> 'b -> 'c) -> 'a -> ('b -> 'c)
(*@ r = f x y z *)

(* ERROR:
   Line 2
   named parameter not specified in function header
   add ~ before x in line 2 *)
