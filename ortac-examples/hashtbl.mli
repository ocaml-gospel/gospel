(*@ open Sequence *)

type ('a, 'b) t
(*@ mutable model : ('a * 'b) sequence *)

val create : int -> ('a, 'b) t
(*@ h = create i
    ensures h = empty *)

val add : ('a, 'b) t -> 'a -> 'b -> unit
(*@ add h a b
    modifies h

    (* describe the new state of h *)
    ensures h = cons (a, b) (old h) *)

exception Not_found

val find_exn : ('a, 'b) t -> 'a -> 'b
(*@ b = find_exn h a

    (* making sure a is bound in h for normal behaviour -- checked as postond
     *)
    ensures _exists (fun x -> fst x = a) h

    (* describe the expected value in normal behaviour *)
    (* should be improved by adding association sequence in stdlib *)
    ensures b = snd (hd (filter (fun x -> fst x = a) h))

    raises Not_found
      (* guarantee that if we enter the exceptional behaviour, a was not bound
       *)
      ensures _forall (fun x -> fst x = a) h
    *)

val find_opt : ('a, 'b) t -> 'a -> 'b option
(*@ o = find_opt h a
    ensures o = if decide (_exists (fun x -> fst x = a) h)
                then Some (snd (hd (filter (fun x -> fst x = a) h)))
                else None *)

(* helper function that could be defined in stdlib if we were to add
   association sequence *)
(*@ function rec remove_first (x: 'a) (xs : ('a * 'b) sequence) : ('a * 'b) sequence =
      if decide (is_empty xs)
      then xs
      else if decide (fst (hd xs) = x)
           then tl xs
           else cons (hd xs) (remove_first x (tl xs)) *)

val mem : ('a, 'b) t -> 'a -> bool
(*@ b = mem h a
    ensures b = _exists (fun x -> fst x = a) h *)

val remove : ('a, 'b) t -> 'a -> unit
(*@ remove h a
    modifies h
    ensures h = remove_first a (old h) *)
