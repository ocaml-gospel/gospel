val f : ('a -> 'b -> 'c) -> 'a -> ('b -> 'c)
(*@ r = f x y *)

(* ERROR:
   Line 2
   insufficient parameters in function header
   add another parameter after y in line 2 *)
