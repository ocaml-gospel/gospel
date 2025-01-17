(*@ function powm (x y m: integer) : integer = mod (pow x y) m *)

(*@ predicate is_sorted (a: int sequence) =
      forall i j. 0 <= i <= j < Sequence.length a
                  -> a[i].v <= a[j].v *)

val merge : int array -> int array -> int array
(*@ c = merge a b
    requires is_sorted a.array_content
    requires is_sorted b.array_content
    ensures is_sorted c.array_content *)

(*@ predicate rec is_sorted_list (l: int list) = match l with
      | [] | _ :: [] -> true
      | h :: (y :: _ as t) -> h.v <= y.v /\ is_sorted_list t *)

(*@ function rec fibonacci (n: integer) : integer =
      if n <= 1 then n else fibonacci (n-2) + fibonacci (n-1) *)
(*@ requires n >= 0
    variant n
    ensures result >= 0 *)
