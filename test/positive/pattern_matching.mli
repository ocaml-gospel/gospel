
type t = O | S of t

val succ: t -> t
  (*@ y = succ x
      ensures y = S x *)

val test1: t -> t -> t
  (*@ r = test1 x y
      requires x <> O && y = O
      ensures  match x, y with
               | _, S _ -> false
               | O, _   -> false
               | S a, O -> r = S (S a) *)
