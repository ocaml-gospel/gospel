type 'a t

val f : 'a t -> 'a t -> ('a -> 'a -> bool) -> bool
(*@ equality *)
