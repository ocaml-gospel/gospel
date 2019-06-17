type t = {x:int}

(*@ function f (x: t): int =
    match x with
    | {x=y} -> x *)

(* ERROR:
   Line 5
   x is of type t and not int
   replace x by y in the term of line 5 *)
