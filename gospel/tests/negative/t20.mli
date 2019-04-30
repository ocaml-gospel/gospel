val f : ?y:int -> int -> int
(*@ r = f ~y x*)

(* ERROR:
   Line 2
   the first parameter is optional but named in spec header
   replace ~ by ? line 2 *)
