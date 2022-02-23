type t = O | S of t

val succ : t -> t
(*@ y = succ x
    ensures y = S x *)

val test1 : t -> t -> t
(*@ r = test1 x y
    requires x <> O && y = O
    ensures  match x, y with
             | _, S _ -> false
             | O, _   -> false
             | S a, O -> r = S (S a) *)

(* pattern of type unit *)
val f_unit : int array -> unit
(*@ x1 = f_unit a
     modifies a
     ensures match x1 with () -> true *)

(*@ function fun_unit (x: unit): string =
    match x with
    | () -> "out" *)

type t2 = B of int * int

(*@ function f (x: t2) : unit =
    match x with B _ -> () *)
