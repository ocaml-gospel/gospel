type t = {x:int}

(*@ function f (x: t): int =
    match x with
    | {x;z} -> x *)

(* ERROR:
   Line 5
   no record with field z
   erase field z in pattern line 5 *)
