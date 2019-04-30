val f : ('a -> 'b -> 'c) -> 'a -> ('b -> 'c)
(*@ r = f x y z w *)

(* ERROR:
   Line 2
   too many parameters in function header
   add one parameter less in line 2 *)
