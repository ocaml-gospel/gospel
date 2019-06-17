type 'a t = {x:int; y:'a}

(*@ function f (x: 'a t): int =
    match x with
    | {x=x;y=x} -> x *)

(* ERROR:
   Line 3
   variable x is duplicated in pattern of line 5
   replace one of the variables in line 5 by other name *)
