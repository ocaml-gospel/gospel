val f : ?y:int -> int -> int
(*@ r = f y x *)

(* ERROR:
   Line 2
   the first parameter should be marked as optional in spec header
   add ~ before y in line 2 *)
