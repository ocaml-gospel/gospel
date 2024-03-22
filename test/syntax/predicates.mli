(*@ function powm (x y m: integer) : integer = mod (pow x y) m *)

(*@ predicate is_sorted (a: int array) =
      forall i j. 0 <= i <= j < Array.length a
                  -> a.(i) <= a.(j) *)

val merge : int array -> int array -> int array
(*@ c = merge a b
    requires is_sorted a
    requires is_sorted b
    ensures is_sorted c *)

(*@ predicate rec is_sorted_list (l: int list) = match l with
      | [] | _ :: [] -> true
      | h :: (y :: _ as t) -> h <= y /\ is_sorted_list t *)

(*@ function rec fibonacci (n: integer) : integer =
      if n <= 1 then n else fibonacci (n-2) + fibonacci (n-1) *)
(*@ requires n >= 0
    variant n
    ensures result >= 0 *)
