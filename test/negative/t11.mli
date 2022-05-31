(*@ function rec f (x: bool) (y: int): bool = f x y *)

(*@ function g (a: int): integer =
      if (f true 2) then 1 else 2 *)

(* ERROR: only if there is no coercion. Type mysmatch integer and int *)

(* EXPECTED
   [125] File "t11.mli", line 4, characters 17-18:
         Error: This term has type `integer' but a term was expected of type `int'.
*)
