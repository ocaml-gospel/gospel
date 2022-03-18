type 'a t

val f : (int -> int -> bool) -> 'a t -> 'a t -> bool
(*@ equality *)
