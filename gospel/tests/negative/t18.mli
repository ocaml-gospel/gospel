val f : z:int -> int -> int
(*@ r = f ~x y*)

(* ERROR:
   Line 2
   first parameter does not match the name in the type
   remove replace x by z in line 2 *)
