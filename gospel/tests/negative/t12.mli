
(*@ function rec f (x: bool) (y: int): bool = f x y *)

(*@ function g (a: int): float =
      if (f true a) then 1. else 2. *)

(*@ function int_of_integer (x:integer): int *)

(*@ function h (a:int) (b:bool) (c:'a): bool =
      if a = int_of_integer 2
      then f b (int_of_integer 3)
      else g (int_of_integer 4) = (int_of_integer 5)
 *)

(* ERROR:
   Line 12
   type mysmatch float and int
   replace "int_of_integer 5" by "5." in line 12 *)
