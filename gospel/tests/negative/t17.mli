val f : int -> int -> int
(*@ r = f ~x y*)

(* ERROR:
   Line 2
   first parameter is not named
   remove ~ before x in line 2 *)
