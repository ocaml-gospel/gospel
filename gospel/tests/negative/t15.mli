
type 'a t2 = C2 of 'a
           | C3 of bool
           | C4 of int * 'a

(*@ function gnr: 'a *)

(*@ function g (x y:'a) (i: int): 'a *)

(*@ function f (x: 'a t2): 'a =
    match x with
    | C2 x -> true
    | C3 b -> gnr
    | C4 (i,x) -> g x x i *)

(* ERROR:
   Line 12
   type mysmatch bool with 'a
   replace true by x in line 12 *)
