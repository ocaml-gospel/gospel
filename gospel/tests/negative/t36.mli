type 'a t = {x:int; y:'a}

(*@ function f (x: t): int =
    match x with
    | {x=x;y=x} -> x *)

(* ERROR:
   Line 3
   type t must have one argument
   add one argument to type t in line 3 *)
