type t = {x:int}

(*@ function f (x: t): int =
    match x with
    | {y} -> y *)

(* ERROR:
   Line 5
   no record with field y
   replace y by x in line 5 *)
