
(*@ function rec f (x: bool) (y: int): bool = f x y *)

(*@ function g (a: int): integer =
      if (f true 2) then 1 else 2 *)

(* ERROR: only if there is no coercion. Type mysmatch integer and int *)
