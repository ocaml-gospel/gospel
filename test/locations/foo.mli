(* beginning of the module *)

type 'a t
(** documentation *)
(*@ mutable model contents : 'a list
    model size : int *)

val create : int -> 'a t
(*@ t = create i
    checks i >= 0
    ensures t.contents = []
    ensures t.size = i *)

(* comments *)

(*@ axiom a : true *)

(*@ function is_full (xs : 'a list) (x : integer) : bool = List.length xs = x *)

(*@ function with_spec (x : integer) (xs : integer list) : integer list *)
(*@ requires not (List.mem x xs)
    ensures true *)

(*@ predicate rec is_sorted_list (l: int list) = match l with
      | [] | _ :: [] -> true
      | h :: (y :: _ as t) -> h <= y /\ is_sorted_list t *)

val add : 'a -> 'a t -> unit
(*@ add a t
    modifies t.contents
    (* comments *)
    ensures t.contents = if is_full t.contents t.size
                         then old t.contents
                         else a :: (old t.contents) *)
