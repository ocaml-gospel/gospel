
type 'a t1 = C of 'a * int

type 'a t2 = T of 'a * int

(*@ function i (a:'a t1): int =
      match a with
      | T (_,y) -> y
      *)

(* ERROR:
   Line 8
   Pattern mysmatch 'a t1 with 'b t2
   Replace T by C in line 8
 *)
