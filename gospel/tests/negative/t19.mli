val f : y:int -> int -> int
(*@ r = f ~y y*)

(* ERROR:
   Line 2
   duplicated vars in val header
   remove replace the second y by z in line 2 *)
