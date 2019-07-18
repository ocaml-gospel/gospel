type 'a t1 = {x:int; y:'a}

type ('a,'b) t2 = 'a t1

(*@ function f (x: 'a t1): ('b,int) t2 =
    match x with
    | {x;y} -> {x;y} *)

(* ERROR:
   Line 5
   cannot match ('b,int) t2 with 'a t1
   replace 'b by 'a in the return type of function f *)
