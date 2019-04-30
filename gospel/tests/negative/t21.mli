val f : y:int -> int -> int
(*@ r = f ?y x*)

(* ERROR:
   Line 2
   the first parameter is named but is defined as optional in spec header
   replace ~ by ? line 2 *)
